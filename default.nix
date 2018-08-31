let
  localLib = import ./lib.nix;
  jemallocOverlay = self: super: {
    # jemalloc has a bug that caused cardano-sl-db to fail to link (via
    # rocksdb, which can use jemalloc).
    # https://github.com/jemalloc/jemalloc/issues/937
    # Using jemalloc 510 with the --disable-initial-exec-tls flag seems to
    # fix it.
    jemalloc = self.callPackage ./nix/jemalloc/jemalloc510.nix {};
  };
in
{ system ? builtins.currentSystem
, config ? {}
, gitrev ? localLib.commitIdFromGitRepo ./.git
, buildId ? null
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; overlays = [ jemallocOverlay ]; })
# profiling slows down performance by 50% so we don't enable it by default
, forceDontCheck ? false
, enableProfiling ? false
, enableDebugging ? false
, enableBenchmarks ? true
, enablePhaseMetrics ? true
, allowCustomConfig ? true
, useStackBinaries ? false
}:

with pkgs.lib;
with pkgs.haskell.lib;

let

  # Overlay logic for *haskell* packages.
  requiredOverlay  = import ./nix/overlays/required.nix   pkgs localLib enableProfiling;
  benchmarkOverlay = import ./nix/overlays/benchmark.nix  pkgs localLib;
  debugOverlay     = import ./nix/overlays/debug.nix      pkgs;
  dontCheckOverlay = import ./nix/overlays/dont-check.nix pkgs;
  metricOverlay    = import ./nix/overlays/metric.nix     pkgs;

  # This will yield a set of haskell packages, based on the given compiler.
  cardanoPkgsBase = ((import ./pkgs { inherit pkgs; }).override {
    ghc = pkgs.haskell.compiler.ghc843;
  });

  activeOverlays = [ requiredOverlay ]
      ++ optional enablePhaseMetrics metricOverlay
      ++ optional enableBenchmarks benchmarkOverlay
      ++ optional enableDebugging debugOverlay
      ++ optional forceDontCheck dontCheckOverlay;

  cardanoPkgs = builtins.foldl' (pkgs: overlay: pkgs.extend overlay) cardanoPkgsBase activeOverlays;

  # Further customizations

  connect = let
      walletConfigFile = ./custom-wallet-config.nix;
      walletConfig = if allowCustomConfig then (if builtins.pathExists walletConfigFile then import walletConfigFile else {}) else {};
    in
      args: pkgs.callPackage ./scripts/launch/connect-to-cluster (args // { inherit gitrev useStackBinaries; } // walletConfig );
  other = rec {
    testlist = innerClosePropagation [] [ cardanoPkgs.cardano-sl ];
    walletIntegrationTests = pkgs.callPackage ./scripts/test/wallet/integration { inherit gitrev useStackBinaries; };
    validateJson = pkgs.callPackage ./tools/src/validate-json {};
    demoCluster = pkgs.callPackage ./scripts/launch/demo-cluster { inherit gitrev useStackBinaries; };
    demoClusterDaedalusDev = pkgs.callPackage ./scripts/launch/demo-cluster { inherit gitrev useStackBinaries; disableClientAuth = true; numImportedWallets = 0; };
    demoClusterLaunchGenesis = pkgs.callPackage ./scripts/launch/demo-cluster {
      inherit gitrev useStackBinaries;
      launchGenesis = true;
      configurationKey = "testnet_full";
      runWallet = false;
    };
    tests = let
      src = localLib.cleanSourceTree ./.;
    in {
      shellcheck = pkgs.callPackage ./scripts/test/shellcheck.nix { inherit src; };
      hlint = pkgs.callPackage ./scripts/test/hlint.nix { inherit src; };
      stylishHaskell = pkgs.callPackage ./scripts/test/stylish.nix { inherit (cardanoPkgs) stylish-haskell; inherit src localLib; };
      walletIntegration = pkgs.callPackage ./scripts/test/wallet/integration/build-test.nix { inherit walletIntegrationTests; };
      swaggerSchemaValidation = pkgs.callPackage ./scripts/test/wallet/swaggerSchemaValidation.nix { inherit gitrev; };
    };
    cardano-sl-explorer-frontend = (import ./explorer/frontend {
      inherit system config gitrev pkgs;
      cardano-sl-explorer = cardanoPkgs.cardano-sl-explorer-static;
    });
    all-cardano-sl = pkgs.buildEnv {
      name = "all-cardano-sl";
      paths = attrValues (filterAttrs (name: drv: localLib.isCardanoSL name) cardanoPkgs);
      ignoreCollisions = true;
    };
    mkDocker = { environment, connectArgs ? {} }: import ./docker.nix { inherit environment connect gitrev pkgs connectArgs; };

    # fixup stack2nix to support a more recent cabal2nix, such that we can parse ...OR... licenses.
    stack2nix = import (pkgs.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "stack2nix";
      rev = "e88a8ab79cf2d354735f546f40690e5607894d46";
      sha256 = "0wmhkfwgrhjbz7s01mli12xhxcqmrr9yz3b72pwv3qnk56cjhx9s";
    }) { inherit pkgs; };
    inherit (pkgs) purescript;
    connectScripts = {
      mainnet = {
        wallet = connect { };
        explorer = connect { executable = "explorer"; };
      };
      staging = {
        wallet = connect { environment = "mainnet-staging"; };
        explorer = connect { executable = "explorer"; environment = "mainnet-staging"; };
      };
      testnet = {
        wallet = connect { environment = "testnet"; };
        explorer = connect { executable = "explorer"; environment = "testnet"; };
      };
      demoWallet = connect { environment = "demo"; };
    };
    dockerImages = {
      mainnet.wallet = mkDocker { environment = "mainnet"; };
      staging.wallet = mkDocker { environment = "mainnet-staging"; };
      testnet.wallet = mkDocker { environment = "testnet"; };
    };
    acceptanceTests = let
      acceptanceTest = pkgs.callPackage ./scripts/test/acceptance;
      mkTest = { environment, ...}: {
        full  = acceptanceTest { inherit environment; resume = false; };
        quick = acceptanceTest { inherit environment; resume = true; };
      };
    in localLib.forEnvironments mkTest;

    cardano-sl-config = pkgs.runCommand "cardano-sl-config" {} ''
      mkdir -p $out/lib
      cp -R ${./log-configs} $out/log-configs
      cp ${./lib}/configuration.yaml $out/lib
      cp ${./lib}/*genesis*.json $out/lib
    '';
    daedalus-bridge = let
      inherit (cardanoPkgs.cardano-sl-node) version;
    in pkgs.runCommand "cardano-daedalus-bridge-${version}" {
      inherit version gitrev buildId;
    } ''
      # Generate daedalus-bridge
      mkdir -p $out/bin
      cd $out
      ${optionalString (buildId != null) "echo ${buildId} > build-id"}
      echo ${gitrev} > commit-id
      echo ${version} > version

      cp --no-preserve=mode -R ${cardano-sl-config}/lib config
      cp ${cardano-sl-config}/log-configs/daedalus.yaml $out/config/log-config-prod.yaml
      cp ${cardanoPkgs.cardano-sl-tools}/bin/cardano-launcher bin
      cp ${cardanoPkgs.cardano-sl-tools}/bin/cardano-x509-certificates bin
      cp ${cardanoPkgs.cardano-sl-wallet-new}/bin/cardano-node bin

      # test that binaries exit with 0
      ./bin/cardano-node --help > /dev/null
      HOME=$TMP ./bin/cardano-launcher --help > /dev/null
    '';
  };
in cardanoPkgs // other
