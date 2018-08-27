{-# LANGUAGE RankNTypes #-}
-- | Default configuration for running tests.

module Test.Pos.Configuration
       ( defaultTestConf
       , defaultTestGenesisSpec
       , defaultTestBlockVersionData

       , HasStaticConfigurations
       , withDefConfiguration
       , withDefNtpConfiguration
       , withDefNodeConfiguration
       , withDefSscConfiguration
       , withDefUpdateConfiguration
       , withDefBlockConfiguration
       , withDefDlgConfiguration
       , withDefConfigurations
       , withStaticConfigurations
       , withProvidedMagicConfig

       , HasConfigurations
       ) where

import           Universum

import qualified Data.Aeson as J
import qualified Data.Set as Set

import           Ntp.Client (NtpConfiguration)

import           Pos.Chain.Block (HasBlockConfiguration, withBlockConfiguration)
import           Pos.Chain.Delegation (HasDlgConfiguration,
                     withDlgConfiguration)
import           Pos.Chain.Ssc (HasSscConfiguration, withSscConfiguration)
import           Pos.Chain.Txp (TxpConfiguration (..))
import           Pos.Chain.Update (HasUpdateConfiguration,
                     withUpdateConfiguration)
import           Pos.Configuration (HasNodeConfiguration, withNodeConfiguration)
import           Pos.Core (HasConfiguration, withGenesisSpec)
import           Pos.Core.Configuration (CoreConfiguration (..),
                     GenesisConfiguration (..))
import           Pos.Core.Genesis (GenesisProtocolConstants (..),
                     GenesisSpec (..))
import           Pos.Core.NetworkMagic (NetworkMagic, RequiresNetworkMagic)
import           Pos.Core.Update (BlockVersionData)
import           Pos.Crypto (ProtocolMagic)
import           Pos.Launcher.Configuration (Configuration (..),
                     HasConfigurations)
import           Pos.Util.Config (embedYamlConfigCT)

-- | This configuration is embedded into binary and is used by default
-- in tests.
defaultTestConf :: Configuration
defaultTestConf = case J.fromJSON $ J.Object jobj of
              J.Error str    -> error $ toText str
              J.Success conf -> conf
  where
    jobj = $(embedYamlConfigCT (Proxy @J.Object) "configuration.yaml" "configuration.yaml" "test")

defaultTestGenesisSpec :: GenesisSpec
defaultTestGenesisSpec =
    case ccGenesis (ccCore defaultTestConf) of
        GCSpec spec -> spec
        _           -> error "unexpected genesis type in test"

defaultTestBlockVersionData :: BlockVersionData
defaultTestBlockVersionData = gsBlockVersionData defaultTestGenesisSpec

-- | This constraint requires all configurations which are not
-- always hardcoded in tests (currently).
type HasStaticConfigurations =
    ( HasUpdateConfiguration
    , HasSscConfiguration
    , HasBlockConfiguration
    , HasNodeConfiguration
    , HasDlgConfiguration
    )

withDefNodeConfiguration :: (HasNodeConfiguration => r) -> r
withDefNodeConfiguration = withNodeConfiguration (ccNode defaultTestConf)

withDefSscConfiguration :: (HasSscConfiguration => r) -> r
withDefSscConfiguration = withSscConfiguration (ccSsc defaultTestConf)

withDefUpdateConfiguration :: (HasUpdateConfiguration => r) -> r
withDefUpdateConfiguration = withUpdateConfiguration (ccUpdate defaultTestConf)

withDefNtpConfiguration :: (NtpConfiguration -> r) -> r
withDefNtpConfiguration fn = fn (ccNtp defaultTestConf)

withDefBlockConfiguration :: (HasBlockConfiguration => r) -> r
withDefBlockConfiguration = withBlockConfiguration (ccBlock defaultTestConf)

withDefDlgConfiguration :: (HasDlgConfiguration => r) -> r
withDefDlgConfiguration = withDlgConfiguration (ccDlg defaultTestConf)

withDefConfiguration :: (HasConfiguration => ProtocolMagic -> NetworkMagic -> r) -> r
withDefConfiguration = withGenesisSpec 0 (ccCore defaultTestConf) id

withProvidedMagicConfig :: ProtocolMagic -> RequiresNetworkMagic -> (HasConfigurations => TxpConfiguration -> r) -> r
withProvidedMagicConfig pm rnm f = withGenesisSpec 0 (updateCC (ccCore defaultTestConf)) id (\_pm _nm -> withStaticConfigurations (\txpConfig _ -> f txpConfig))
  where
    updateCC :: CoreConfiguration -> CoreConfiguration
    updateCC cc = cc { ccGenesis = updateGC (ccGenesis cc) }
    --
    updateGC :: GenesisConfiguration -> GenesisConfiguration
    updateGC (GCSrc _ _) = error "got GCSrc"
    updateGC (GCSpec spec) = GCSpec $ spec
        { gsProtocolConstants = updateGPC (gsProtocolConstants spec) }
    --
    updateGPC :: GenesisProtocolConstants -> GenesisProtocolConstants
    updateGPC gpc = gpc { gpcProtocolMagic = pm
                        , gpcRequiresNetworkMagic = rnm }

withStaticConfigurations :: (HasStaticConfigurations => TxpConfiguration -> NtpConfiguration -> r) -> r
withStaticConfigurations patak =
    withDefNodeConfiguration $
    withDefSscConfiguration $
    withDefUpdateConfiguration $
    withDefBlockConfiguration $
    withDefDlgConfiguration $
    withDefNtpConfiguration (patak $ TxpConfiguration 200 Set.empty)

withDefConfigurations
    :: (HasConfigurations => ProtocolMagic -> NetworkMagic -> TxpConfiguration -> NtpConfiguration -> r) -> r
withDefConfigurations bardaq =
    withDefConfiguration $ \pm nm -> withStaticConfigurations (bardaq pm nm)
