{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Spec.Keystore (
    spec
  ) where

import           Universum

import           System.Directory (doesFileExist, removeFile)
import           System.IO.Error (IOError)
import           Test.Hspec (Spec, describe, it, shouldBe, shouldReturn,
                     shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Gen, arbitrary)
import           Test.QuickCheck.Monadic (forAllM, monadicIO, pick, run)

import           Cardano.Wallet.Kernel.DB.HdWallet (eskToHdRootId)
import           Cardano.Wallet.Kernel.Keystore (DeletePolicy (..), Keystore)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.Types (WalletId (..))
import           Pos.Core.NetworkMagic (NetworkMagic, RequiresNetworkMagic (..), makeNetworkMagic)
import           Pos.Crypto (EncryptedSecretKey, hash, safeKeyGen)
import           Util.Buildable (ShowThroughBuild (..))

import           Test.Pos.Crypto.Dummy (dummyProtocolMagic)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | Creates and operate on a keystore. The 'Keystore' is created in a temporary
-- directory and garbage-collected from the Operating System.
withKeystore :: (Keystore -> IO a) -> IO a
withKeystore = Keystore.bracketTestKeystore

genKeypair
    :: NetworkMagic
    -> Gen ( ShowThroughBuild WalletId
           , ShowThroughBuild EncryptedSecretKey
           )
genKeypair nm = do
    (_, esk) <- arbitrary >>= safeKeyGen
    return $ bimap STB STB (WalletIdHdRnd . (eskToHdRootId nm)  $ esk, esk)

genKeys
    :: NetworkMagic
    -> Gen ( ShowThroughBuild WalletId
           , ShowThroughBuild EncryptedSecretKey
           , ShowThroughBuild EncryptedSecretKey
           )
genKeys nm = do
    (wId, origKey) <- genKeypair nm
    (_, esk2) <- arbitrary >>= safeKeyGen
    return (wId, origKey, STB esk2)

nukeKeystore :: FilePath -> IO ()
nukeKeystore fp =
    removeFile fp `catch` (\(_ :: IOError) -> return ())

spec :: Spec
spec = do
    go NMMustBeNothing
    go NMMustBeJust
  where
    go rnm = describe "Keystore to store UserSecret(s)" $ do
        let nm = makeNetworkMagic rnm dummyProtocolMagic
        it "creating a brand new one works" $ do
            nukeKeystore "test_keystore.key"
            Keystore.bracketKeystore KeepKeystoreIfEmpty "test_keystore.key" $ \_ks ->
                return ()
            doesFileExist "test_keystore.key" `shouldReturn` True

        it "destroying a keystore (completely) works" $ do
            nukeKeystore "test_keystore.key"
            Keystore.bracketKeystore RemoveKeystoreIfEmpty "test_keystore.key" $ \_ks ->
                return ()
            doesFileExist "test_keystore.key" `shouldReturn` False

        prop "lookup of keys works" $ monadicIO $ do
            forAllM (genKeypair nm) $ \(STB wid, STB esk) -> run $ do
                withKeystore $ \ks -> do
                    Keystore.insert wid esk ks
                    mbKey <- Keystore.lookup nm wid ks
                    (fmap hash mbKey) `shouldBe` (Just (hash esk))

        prop "replacement of keys works" $ monadicIO $ do
            forAllM (genKeys nm) $ \(STB wid, STB oldKey, STB newKey) -> run $ do
                withKeystore $ \ks -> do
                    Keystore.insert wid oldKey ks
                    mbOldKey <- Keystore.lookup nm wid ks
                    Keystore.replace nm wid newKey ks
                    mbNewKey <- Keystore.lookup nm wid ks
                    (fmap hash mbOldKey) `shouldSatisfy` ((/=) (fmap hash mbNewKey))

        prop "Inserts are persisted after releasing the keystore" $ monadicIO $ do
            (STB wid, STB esk) <- pick $ genKeypair nm
            run $ do
                nukeKeystore "test_keystore.key"
                Keystore.bracketKeystore KeepKeystoreIfEmpty "test_keystore.key" $ \keystore1 ->
                    Keystore.insert wid esk keystore1
                Keystore.bracketKeystore KeepKeystoreIfEmpty "test_keystore.key" $ \keystore2 -> do
                    mbKey <- Keystore.lookup nm wid keystore2
                    (fmap hash mbKey) `shouldBe` (Just (hash esk))

        prop "deletion of keys works" $ monadicIO $ do
            forAllM (genKeypair nm) $ \(STB wid, STB esk) -> run $ do
                withKeystore $ \ks -> do
                    Keystore.insert wid esk ks
                    Keystore.delete nm wid ks
                    mbKey <- Keystore.lookup nm wid ks
                    (fmap hash mbKey) `shouldBe` Nothing

        prop "Deletion of keys are persisted after releasing the keystore" $ monadicIO $ do
            (STB wid, STB esk) <- pick $ genKeypair nm
            run $ do
                nukeKeystore "test_keystore.key"
                Keystore.bracketKeystore KeepKeystoreIfEmpty "test_keystore.key" $ \keystore1 -> do
                    Keystore.insert wid esk keystore1
                    Keystore.delete nm wid keystore1
                Keystore.bracketKeystore KeepKeystoreIfEmpty "test_keystore.key" $ \keystore2 -> do
                    mbKey <- Keystore.lookup nm wid keystore2
                    (fmap hash mbKey) `shouldBe` Nothing
