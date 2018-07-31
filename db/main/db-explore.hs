{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

import           Universum

import           Control.Monad (when)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Data.ByteArray (alloc, withByteArray)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Conduit (ConduitT, awaitForever, runConduit, yield, (.|))
import           Data.Conduit.List (consume, sourceList)
import qualified Data.List as List
import qualified Data.Text as Text
import           Foreign.Storable (peek, poke, sizeOf)
import           Formatting (build, sformat, (%))
import qualified Prelude
import           System.Environment (lookupEnv)
import           System.FilePath (dropExtension)
import           System.IO (IOMode (..), SeekMode (..), hClose, hSeek,
                     openBinaryFile, withBinaryFile)


import           Pos.Chain.Block (HeaderHash, blockHeaderHash)
import           Pos.Core (CoreConfiguration (..), EpochIndex (..),
                     EpochOrSlot (..), GenesisConfiguration (..),
                     HasConfiguration, LocalSlotIndex (..), ProtocolMagic,
                     SlotId (..), getEpochOrSlot, withCoreConfigurations)
import           Pos.Crypto.Hashing (decodeAbstractHash)
import           Pos.DB.Block (dbGetSerBlockRealDefault,
                     dbGetSerUndoRealDefault, getFirstGenesisBlockHash,
                     resolveForwardLink)
import           Pos.DB.BlockIndex (getHeader, getTipHeader)
import           Pos.DB.Class (MonadDBRead (..), Serialized (..),
                     SerializedBlock, dbGetSerBlock)
import           Pos.DB.Rocks.Functions (closeNodeDBs, dbGetDefault,
                     dbIterSourceDefault, openNodeDBs)
import           Pos.DB.Rocks.Types


main :: IO ()
main = do
    dbpath <- fromMaybe (error "Environment variable CARDANO_DB_PATH not defined.")
                <$> lookupEnv "CARDANO_DB_PATH"
    genPath <- fromMaybe (error "Environment variable CARDANO_GENESIS_JSON_DIR not defined.")
                <$> lookupEnv "CARDANO_GENESIS_JSON_DIR"
    coreConfig genPath $ \ _ ->
        bracket (openNodeDBs False dbpath) closeNodeDBs $ \ nbds ->
            runReaderT exploreDB nbds


coreConfig :: FilePath -> (HasConfiguration => ProtocolMagic -> IO r) -> IO r
coreConfig mainnetGenesisPath =
    withCoreConfigurations
        coreConfiguration
        id -- (\ _ -> error "coreConfig: function")
        mainnetGenesisPath
        (Just 1506203091000000)
        Nothing

coreConfiguration :: CoreConfiguration
coreConfiguration =
    -- Need this *exact* configuration or reading the genesis block from the DB will fail.
    CoreConfiguration
        { ccGenesis = GCSrc "mainnet-genesis.json"
            (either error id
                $ decodeAbstractHash "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb")
        , ccDbSerializeVersion = 0
        }

type ExploreDB = ReaderT NodeDBs IO

-- Currently only needed for `getBlockIndexDB` in `exploreDB`.
instance HasConfiguration => MonadDBRead ExploreDB where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault
    dbGetSerBlock = dbGetSerBlockRealDefault
    dbGetSerUndo = dbGetSerUndoRealDefault


exploreDB :: (MonadCatch m, MonadDBRead m, MonadReader NodeDBs m, MonadIO m) => m ()
exploreDB = do
    liftIO $ putStrLn ("Database ok!" :: Text)
    _blocksDB <- getBlockIndexDB
    liftIO $ putStrLn ("Have blockDB" :: Text)

    mtip <- getHeaderEpochOrSlot =<< fmap blockHeaderHash getTipHeader
    printEpochOrSlot "Tip" mtip

    gen <- getFirstGenesisBlockHash
    mGenEos <- getHeaderEpochOrSlot gen
    printEpochOrSlot "Genesis" mGenEos

    when False $ do
        liftIO $ putStrLn ("------------------------" :: Text)
        walkFirstEpoch

    let epochFP = "/home/daedalus/tmp/0000.epoch"
    let indexPath = dropExtension epochFP ++ ".index"

    res <- runExceptT $ do
        (_, bhashs0) <- getHeaderHashesForEpoch gen

        xs <- lift $ consolidateEpochBlocks epochFP bhashs0
        liftIO $ writeEpochIndex indexPath xs

        liftIO $ putStrLn ("------------------------" :: Text)
        testEpochData bhashs0 epochFP indexPath

    liftIO $ either (putStrLn . renderEpochConError) pure res

testEpochData :: (MonadDBRead m, MonadIO m) => [SlotIndexHash] -> FilePath -> FilePath -> ExceptT EpochConError m ()
testEpochData xs epochPath indexPath = do
    mapM_ testBlock xs
    putStrLn ("All blocks are good!" :: Text)
  where
    testBlock (SlotIndexHash lsi hh) = do
        when (getSlotIndex lsi `mod` 100 == 0) $
            putStrLn $ sformat ("Checking slot index " % build % " of " % build) (getSlotIndex lsi) (length xs)
        mblk1 <- unSerialized <$$> dbGetSerBlock hh
        blk1 <- maybe (throwE $ ECBlockLookupFailed "testEpochData" lsi hh) pure mblk1
        blk2 <- unSerialized <$> getEpochBlock epochPath indexPath lsi
        when (blk1 /= blk2) $
            throwE $ ECBlockMismatch "testEpochData" lsi

getEpochBlock :: MonadIO m => FilePath -> FilePath -> LocalSlotIndex -> ExceptT EpochConError m SerializedBlock
getEpochBlock epochPath indexPath lsi = do
    moff <- liftIO $ getEpochBlockOffset indexPath lsi
    off <- maybe (throwE $ ECBOffsetFail "testEpochData") pure moff
    liftIO . withBinaryFile epochPath ReadMode $ \ hdl -> do
        hSeek hdl AbsoluteSeek $ fromIntegral off
        tag <- BS.hGet hdl 4
        when (tag /= "blck") $
          error . Text.pack $ "getEpochBlock: bad tag " ++ show (tag, lsi, off)
        bslen <- BS.hGet hdl 4
        len <- unpackInt32 bslen
        Serialized <$> BS.hGet hdl (fromIntegral len)


walkFirstEpoch :: (MonadCatch m, MonadDBRead m, MonadReader NodeDBs m, MonadIO m) => m ()
walkFirstEpoch = do
    genesisHash <- getFirstGenesisBlockHash
    loop $ Just genesisHash
  where
    loop :: (MonadCatch m, MonadDBRead m, MonadReader NodeDBs m, MonadIO m) => Maybe HeaderHash -> m ()
    loop mHash =
        maybe (pure ()) handleHash mHash

    handleHash :: (MonadCatch m, MonadDBRead m, MonadReader NodeDBs m, MonadIO m) => HeaderHash -> m ()
    handleHash hhash = do
        continue <- reportHash hhash
        when continue $
            loop =<< resolveForwardLink hhash


reportHash :: (MonadCatch m, MonadDBRead m, MonadReader NodeDBs m, MonadIO m) => HeaderHash -> m Bool
reportHash hhash = do
    meos <- getHeaderEpochOrSlot hhash
    liftIO . putStrLn $ sformat (build % "  " % build) hhash meos
    pure $ case meos of
            Nothing -> False
            Just eos -> case unEpochOrSlot eos of
                          Right sid -> getEpochIndex (siEpoch sid) < 2
                          Left _    -> True

data EpochConError
    = ECFinalBlockNotBoundary !Text
    | ECExpectedGenesis !Text !HeaderHash
    | ECExcpectedMain !Text !HeaderHash
    | ECForwardLink !Text !HeaderHash
    | ECEoSLookupFailed !Text !HeaderHash
    | ECBlockLookupFailed !Text !LocalSlotIndex !HeaderHash
    | ECBOffsetFail !Text
    | ECBlockMismatch !Text !LocalSlotIndex

renderEpochConError :: EpochConError -> Text
renderEpochConError = \case
    ECFinalBlockNotBoundary fn ->
        fn <> ": Final block is not an epoch boundary block"
    ECExpectedGenesis fn h ->
        fn <> sformat (": hash " % build % " should be an epoch boundary hash.") h
    ECExcpectedMain fn h ->
        fn <> sformat (": hash " % build % " should be a main block hash.") h
    ECForwardLink fn h ->
        fn <> sformat (": failed to follow hash " % build) h
    ECEoSLookupFailed fn h ->
        fn <> sformat (": EpochOrSlot lookup failed on hash " % build) h
    ECBlockLookupFailed fn lsi h ->
        fn <> sformat (": block lookup failed on (" % build % ", " % build % ")") lsi h
    ECBOffsetFail fn ->
        fn <> ": Failed to find offset"
    ECBlockMismatch fn lsi ->
        fn <> sformat (": block mismatch at index " % build) lsi

data SlotIndexHash
    = SlotIndexHash !LocalSlotIndex !HeaderHash

-- Use sized types here because we store these as binary in the
-- epoch index file.
-- The 'Int32' is the length of the block for the given slot index or
-- -1 if that slot index is empty.
data SlotIndexLength = SlotIndexLength
    { silSlotIndex :: !Word16
    , silLength    :: !Word32
    } deriving Eq

data SlotIndexOffset = SlotIndexOffset
    { sioSlotIndex :: !Word16
    , sioOffset    :: !Int64
    }


-- https://gist.github.com/dcoutts/cbdf941b263687d91a911f79bfbd89a8

-- | Given a '[SlotIndexHash]' representing all the 'HeaderHash's for a given
-- epoch ordered by ascending 'LocalSlotIndex', write out a file containing all
-- the blocks to a single file specified by 'FilePath' and return a
-- '[SlotIndexLength]' which is used to write the epoch index file.
consolidateEpochBlocks :: (MonadReader NodeDBs m, MonadDBRead m, MonadIO m) => FilePath -> [SlotIndexHash] -> m [SlotIndexLength]
consolidateEpochBlocks fpath xs = do
    hdl <- liftIO $ openBinaryFile fpath WriteMode
    liftIO $ BS.hPutStr hdl epochFileHeader
    ys <- runConduit $ sourceList xs .| consolidate hdl .| consume
    liftIO $ hClose hdl
    pure ys
  where
    consolidate :: (HasConfiguration, MonadDBRead m, MonadReader NodeDBs m, MonadIO m) => Handle -> ConduitT SlotIndexHash SlotIndexLength m ()
    consolidate hdl =
        awaitForever $ \(SlotIndexHash lsi hh) -> do
            mbs <- unSerialized <$$> lift (dbGetSerBlock hh)
            len <- liftIO $ do
                case mbs of
                    Nothing -> do
                        putStrLn $ sformat ("consolidate: " % build % " " % build) lsi hh
                        pure 0
                    Just rbs -> do
                        let rbsLen = BS.length rbs
                        blkLen <- packInt32 $ fromIntegral rbsLen
                        LBS.hPutStr hdl $ LBS.fromChunks ["blck", blkLen, rbs]
                        pure $ fromIntegral (rbsLen + 8)
            yield $ SlotIndexLength (getSlotIndex lsi) len


epochFileHeader :: ByteString
epochFileHeader = "Epoch data v1\n"

indexFileHeader :: ByteString
indexFileHeader = "Epoch index v1\n"


packInt32 :: Int32 -> IO ByteString
packInt32 i32 =
    alloc (sizeOf i32) (\p -> poke p i32)

unpackInt32 :: ByteString -> IO Int32
unpackInt32 bs =
    withByteArray bs peek

writeEpochIndex :: FilePath -> [SlotIndexLength] -> IO ()
writeEpochIndex fpath xs =
    -- TODO: Make this production ready.
    withFile fpath WriteMode $ \ hdl -> do
        BS.hPutStr hdl indexFileHeader
        BS.hPutStrLn hdl $ show (map (\x -> (silSlotIndex x, silLength x)) xs)

readEpochIndex :: FilePath -> IO [SlotIndexLength]
readEpochIndex fpath =
    -- TODO: Make this production ready.
    withFile fpath ReadMode $ \ hdl -> do
        _ <- BS.hGetLine hdl
        map (uncurry SlotIndexLength) . Prelude.read . BS.unpack
            <$> BS.hGetContents hdl

epochIndexToOffset :: [SlotIndexLength] -> [SlotIndexOffset]
epochIndexToOffset =
    snd . mapAccumL convert (fromIntegral $ BS.length epochFileHeader)
  where
    convert :: Int64 -> SlotIndexLength -> (Int64, SlotIndexOffset)
    convert offset (SlotIndexLength a b) =
        (offset + fromIntegral b, SlotIndexOffset a offset)

readEpochOffsets :: FilePath -> IO [SlotIndexOffset]
readEpochOffsets fpath =
    epochIndexToOffset <$> readEpochIndex fpath

findEpochBlockOffset :: LocalSlotIndex -> [SlotIndexOffset] -> Maybe Int64
findEpochBlockOffset (UnsafeLocalSlotIndex lsi) xs =
    sioOffset <$> List.find (\x -> sioSlotIndex x == lsi) xs

getEpochBlockOffset :: FilePath -> LocalSlotIndex -> IO (Maybe Int64)
getEpochBlockOffset fpath lsi =
    findEpochBlockOffset lsi <$> readEpochOffsets fpath

-- | Given the hash of an epoch boundary block, return a pair of the next
-- epoch boundary hash and a list of the header hashes of the main blocks
-- between the two boundary blocks.
getHeaderHashesForEpoch :: MonadDBRead m => HeaderHash -> ExceptT EpochConError m (HeaderHash, [SlotIndexHash])
getHeaderHashesForEpoch ghash = do
    mbh <- isMainBlockHeader ghash
    when mbh $
        throwE $ ECExpectedGenesis "getHeaderHashesForEpoch" ghash
    (ng, bhs) <- loop [] ghash
    whenM (isMainBlockHeader ng) $
        throwE $ ECFinalBlockNotBoundary "getHeaderHashesForEpoch"
    pure (ng, reverse bhs)
  where
    loop :: MonadDBRead m => [SlotIndexHash] -> HeaderHash -> ExceptT EpochConError m (HeaderHash, [SlotIndexHash])
    loop !acc hash = do
        mnext <- resolveForwardLink hash
        next <- maybe (throwE $ errorHash hash) pure mnext
        ifM (not <$> isMainBlockHeader next)
            (pure (next, acc))
            (do lsi <- getLocalSlotIndex next
                loop (SlotIndexHash lsi next : acc) next
                )

    errorHash hash =
        ECForwardLink "getHeaderHashesForEpoch" hash


getLocalSlotIndex :: MonadDBRead m => HeaderHash -> ExceptT EpochConError m LocalSlotIndex
getLocalSlotIndex hash = do
    meos <- getHeaderEpochOrSlot hash
    case meos of
        Nothing -> throwE $ ECEoSLookupFailed "getLocalSlotIndex" hash
        Just eos ->
            case unEpochOrSlot eos of
                Left _ -> throwE $ ECExcpectedMain "getLocalSlotIndex" hash
                Right sid -> pure $ siSlot sid

isMainBlockHeader :: MonadDBRead m => HeaderHash -> m Bool
isMainBlockHeader hh =
    maybe False (isRight . unEpochOrSlot) <$> getHeaderEpochOrSlot hh

-- -------------------------------------------------------------------------------------------------

getHeaderEpochOrSlot :: MonadDBRead m => HeaderHash -> m (Maybe EpochOrSlot)
getHeaderEpochOrSlot =
    fmap2 getEpochOrSlot . getHeader


printEpochOrSlot :: MonadIO m => String -> Maybe EpochOrSlot -> m ()
printEpochOrSlot s meos =
    liftIO . putStrLn $
        case meos of
              Just eos -> renderEpochOrSlot s eos
              Nothing  -> "No " ++ s ++ " block"

renderEpochOrSlot :: String -> EpochOrSlot -> String
renderEpochOrSlot s eos =
    case unEpochOrSlot eos of
        Right sid -> s ++ " (epoch, slot): " ++ show (getEpochIndex $ siEpoch sid, getSlotIndex $ siSlot sid)
        Left eid -> s ++ " epoch: " ++ show (getEpochIndex eid)

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap2
