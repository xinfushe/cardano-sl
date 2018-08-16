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
import           Data.Binary (decode, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Either (partitionEithers)
import qualified Data.Text as Text
import           Formatting (build, sformat, (%))
import           System.Environment (lookupEnv)
import           System.FilePath (dropExtension, (</>))
import           System.IO (IOMode (..), SeekMode (..), hClose, hSeek,
                     openBinaryFile, withBinaryFile)


import           Pos.Chain.Block (HeaderHash, blockHeaderHash)
import           Pos.Core (CoreConfiguration (..), EpochIndex (..),
                     EpochOrSlot (..), GenesisConfiguration (..),
                     HasConfiguration, LocalSlotIndex (..), ProtocolMagic,
                     SlotId (..), epochSlots, getEpochOrSlot,
                     withCoreConfigurations)
import           Pos.Crypto.Hashing (decodeAbstractHash)
import           Pos.DB.Block (dbGetSerBlockRealDefault,
                     dbGetSerBlundRealDefault, dbGetSerUndoRealDefault,
                     dbPutSerBlundsRealDefault, getFirstGenesisBlockHash,
                     resolveForwardLink)
import           Pos.DB.BlockIndex (getHeader, getTipHeader)
import           Pos.DB.Class (DBTag (MiscDB), MonadDB (..), MonadDBRead (..),
                     Serialized (..), SerializedBlock, dbGetSerBlock)
import           Pos.DB.Epoch.Index (SlotIndexOffset (..), getEpochBlockOffset,
                     writeEpochIndex)
import           Pos.DB.Misc.Common (miscGetBi, miscPutBi)
import           Pos.DB.Rocks.Functions (closeNodeDBs, dbDeleteDefault,
                     dbGetDefault, dbIterSourceDefault, dbPutDefault,
                     dbWriteBatchDefault, openNodeDBs)
import           Pos.DB.Rocks.Types (NodeDBs (..), epochDataDir, epochLock,
                     getBlockIndexDB, getNodeDBs)
import           Pos.Util.Concurrent.RWLock (whenAcquireWrite)

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
    dbGetSerBlund = dbGetSerBlundRealDefault

instance HasConfiguration => MonadDB ExploreDB where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault
    dbPutSerBlunds = dbPutSerBlundsRealDefault

exploreDB :: (MonadCatch m, MonadDB m, MonadMask m, MonadReader NodeDBs m, MonadIO m) => m ()
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
        when False $ do
            (_, bhashs0) <- getHeaderHashesForEpoch gen

            xs <- consolidateEpochBlocks epochFP bhashs0
            liftIO $ writeEpochIndex epochSlots indexPath xs

            liftIO $ putStrLn ("------------------------" :: Text)
            testEpochData bhashs0 epochFP indexPath

        when False $ do
            liftIO $ putStrLn ("Deleting ConsolidatedEpochHash" :: Text)
            deleteConsolidatedEpochHash

        consoldidateEpochs

    liftIO $ either (putStrLn . renderConsolidateError) pure res


getConsolidatedEpochHash :: MonadDBRead m => m (Maybe HeaderHash)
getConsolidatedEpochHash =
    miscGetBi consolidatedEpochHashKey

putConsolidatedEpochHash :: MonadDB m => HeaderHash -> m ()
putConsolidatedEpochHash hhash =
    miscPutBi consolidatedEpochHashKey hhash

deleteConsolidatedEpochHash :: MonadDB m => m ()
deleteConsolidatedEpochHash =
    dbDelete MiscDB consolidatedEpochHashKey

consolidatedEpochHashKey :: ByteString
consolidatedEpochHashKey = "consolidatedEpochHash"

getNextEpochBoundaryHash :: MonadDBRead m => m HeaderHash
getNextEpochBoundaryHash =
   getConsolidatedEpochHash >>= \case
     Just eh -> pure eh
     Nothing -> getFirstGenesisBlockHash

consoldidateEpochs :: (MonadCatch m, MonadDB m, MonadIO m, MonadMask m, MonadReader NodeDBs m) => ExceptT ConsolidateError m ()
consoldidateEpochs = ExceptT $ do
    elock <- view epochLock <$> getNodeDBs
    mr <- whenAcquireWrite elock $ do
            liftIO $ putStrLn ("Consolidating epochs" :: Text)
            tipEpoch <- getTipEpoch
            startHash <- getNextEpochBoundaryHash
            startEpoch <- getBlockHeaderEpoch startHash
            if tipEpoch < 2
                then pure $ Right ()
                else consoldateEpoch startHash startEpoch (tipEpoch - 1)
    pure $ fromMaybe (Right ()) mr

consoldateEpoch :: (MonadCatch m, MonadDB m, MonadIO m, MonadMask m, MonadReader NodeDBs m) => HeaderHash -> EpochIndex -> EpochIndex -> m (Either ConsolidateError ())
consoldateEpoch startHash startEpoch endEpoch
    | startEpoch >= endEpoch = pure $ Right ()
    | otherwise = runExceptT $ loop startHash startEpoch
  where
    loop :: (MonadCatch m, MonadDB m, MonadIO m, MonadMask m, MonadReader NodeDBs m) => HeaderHash -> EpochIndex -> ExceptT ConsolidateError m ()
    loop hhash epoch = do
        (epochBoundary, hashes) <- getHeaderHashesForEpoch hhash
        (epochPath, indexPath) <- mkEpochPaths epoch . view epochDataDir <$> getNodeDBs
        liftIO $ print (epochPath, indexPath)

        xs <- consolidateEpochBlocks epochPath hashes
        liftIO $ writeEpochIndex epochSlots indexPath xs

        -- Write starting point for next consildation to the MiscDB.
        putConsolidatedEpochHash epochBoundary
        when (epoch < endEpoch) $
            loop epochBoundary (epoch + 1)

mkEpochPaths :: EpochIndex -> FilePath -> (FilePath, FilePath)
mkEpochPaths epoch dir =
    (dir </> epochStr ++ ".epoch", dir </> epochStr ++ ".index")
  where
    epochStr = replicate (5 - epochStrLen) '0' ++ epochShow
    epochShow = show $ getEpochIndex epoch
    epochStrLen = length epochShow


testEpochData :: (MonadDBRead m, MonadIO m) => [SlotIndexHash] -> FilePath -> FilePath -> ExceptT ConsolidateError m ()
testEpochData xs epochPath indexPath = do
    mapM_ testBlock xs
    putStrLn ("All blocks are good!" :: Text)
  where
    testBlock (SlotIndexHash lsi hh) = do
        when (getSlotIndex lsi `mod` 100 == 0) $
            putStrLn $ sformat ("Checking slot index " % build % " of " % build) (getSlotIndex lsi) (length xs)
        mblk1 <- unSerialized <$$> dbGetSerBlund hh
        blk1 <- maybe (throwE $ CEBlockLookupFailed "testEpochData" lsi hh) pure mblk1
        blk2 <- unSerialized <$> getEpochBlock epochPath indexPath lsi
        when (blk1 /= blk2) $
            throwE $ CEBlockMismatch "testEpochData" lsi

getEpochBlock :: MonadIO m => FilePath -> FilePath -> LocalSlotIndex -> ExceptT ConsolidateError m SerializedBlock
getEpochBlock epochPath indexPath lsi = do
    moff <- liftIO $ getEpochBlockOffset indexPath lsi
    off <- maybe (throwE $ CEBOffsetFail "testEpochData") pure moff
    liftIO . withBinaryFile epochPath ReadMode $ \ hdl -> do
        hSeek hdl AbsoluteSeek $ fromIntegral off
        tag <- BS.hGet hdl 4
        when (tag /= "blnd") $
          error . Text.pack $ "getEpochBlock: bad tag " ++ show (tag, lsi, off)
        bslen <- BS.hGet hdl 4
        Serialized <$> BS.hGet hdl (fromIntegral $ unpackWord32 bslen)


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

data ConsolidateError
    = CEFinalBlockNotBoundary !Text
    | CEExpectedGenesis !Text !HeaderHash
    | CEExcpectedMain !Text !HeaderHash
    | CEForwardLink !Text !HeaderHash
    | CEEoSLookupFailed !Text !HeaderHash
    | CEBlockLookupFailed !Text !LocalSlotIndex !HeaderHash
    | CEBOffsetFail !Text
    | CEBlockMismatch !Text !LocalSlotIndex
    | CEBBlockNotFound !Text !LocalSlotIndex !HeaderHash

renderConsolidateError :: ConsolidateError -> Text
renderConsolidateError = \case
    CEFinalBlockNotBoundary fn ->
        fn <> ": Final block is not an epoch boundary block"
    CEExpectedGenesis fn h ->
        fn <> sformat (": hash " % build % " should be an epoch boundary hash.") h
    CEExcpectedMain fn h ->
        fn <> sformat (": hash " % build % " should be a main block hash.") h
    CEForwardLink fn h ->
        fn <> sformat (": failed to follow hash " % build) h
    CEEoSLookupFailed fn h ->
        fn <> sformat (": EpochOrSlot lookup failed on hash " % build) h
    CEBlockLookupFailed fn lsi h ->
        fn <> sformat (": block lookup failed on (" % build % ", " % build % ")") lsi h
    CEBOffsetFail fn ->
        fn <> ": Failed to find offset"
    CEBlockMismatch fn lsi ->
        fn <> sformat (": block mismatch at index " % build) lsi
    CEBBlockNotFound fn lsi hh ->
        fn <> sformat (": block mssing : " % build % " " % build) lsi hh

data SlotIndexHash
    = SlotIndexHash !LocalSlotIndex !HeaderHash

-- Use sized types here because we store these as binary in the
-- epoch index file.
-- The 'Word32' is the length of the block for the given slot index or
-- -1 if that slot index is empty.
data SlotIndexLength
    = SlotIndexLength !Word16 !Word32
    deriving Eq


-- https://gist.github.com/dcoutts/cbdf941b263687d91a911f79bfbd89a8

-- | Given a '[SlotIndexHash]' representing all the 'HeaderHash's for a given
-- epoch ordered by ascending 'LocalSlotIndex', write out a file containing all
-- the blocks to a single file specified by 'FilePath' and return a
-- '[SlotIndexLength]' which is used to write the epoch index file.
consolidateEpochBlocks :: (MonadReader NodeDBs m, MonadDBRead m, MonadIO m, MonadMask m) => FilePath -> [SlotIndexHash] -> ExceptT ConsolidateError m [SlotIndexOffset]
consolidateEpochBlocks fpath xs = ExceptT $ do
    ys <- bracket
            (liftIO $ openBinaryFile fpath WriteMode)
            (liftIO . hClose)
            (\hdl -> do
                liftIO $ BS.hPutStr hdl epochFileHeader
                mapM (consolidate hdl) xs
                )
    pure $ case partitionEithers ys of
            ([], zs) -> Right $ epochIndexToOffset zs
            (e:_, _) -> Left e
  where
    consolidate :: (HasConfiguration, MonadDBRead m, MonadReader NodeDBs m, MonadIO m) => Handle -> SlotIndexHash -> m (Either ConsolidateError SlotIndexLength)
    consolidate hdl  (SlotIndexHash lsi hh) = do
        mbs <- unSerialized <$$> dbGetSerBlund hh
        case mbs of
            Nothing ->
                pure . Left $ CEBBlockNotFound "consolidateEpochBlocks" lsi hh
            Just rbs -> do
                let rbsLen = BS.length rbs
                liftIO $ do
                    let blkLen = packWord32 $ fromIntegral rbsLen
                    LBS.hPutStr hdl $ LBS.fromChunks ["blnd", blkLen, rbs]
                pure . Right $ SlotIndexLength (getSlotIndex lsi) (fromIntegral $ rbsLen + 8)


epochFileHeader :: ByteString
epochFileHeader = "Epoch data v1\n"

packWord32 :: Word32 -> ByteString
packWord32 = LBS.toStrict . encode

unpackWord32 :: ByteString -> Word32
unpackWord32 = decode . LBS.fromStrict

epochIndexToOffset :: [SlotIndexLength] -> [SlotIndexOffset]
epochIndexToOffset =
    snd . mapAccumL convert (fromIntegral $ BS.length epochFileHeader)
  where
    convert :: Word64 -> SlotIndexLength -> (Word64, SlotIndexOffset)
    convert offset (SlotIndexLength a b) =
        (offset + fromIntegral b, SlotIndexOffset a offset)


-- | Given the hash of an epoch boundary block, return a pair of the next
-- epoch boundary hash and a list of the header hashes of the main blocks
-- between the two boundary blocks.
getHeaderHashesForEpoch :: MonadDBRead m => HeaderHash -> ExceptT ConsolidateError m (HeaderHash, [SlotIndexHash])
getHeaderHashesForEpoch ghash = do
    mbh <- isMainBlockHeader ghash
    when mbh $
        throwE $ CEExpectedGenesis "getHeaderHashesForEpoch" ghash
    (ng, bhs) <- loop [] ghash
    whenM (isMainBlockHeader ng) $
        throwE $ CEFinalBlockNotBoundary "getHeaderHashesForEpoch"
    pure (ng, reverse bhs)
  where
    loop :: MonadDBRead m => [SlotIndexHash] -> HeaderHash -> ExceptT ConsolidateError m (HeaderHash, [SlotIndexHash])
    loop !acc hash = do
        mnext <- resolveForwardLink hash
        next <- maybe (throwE $ errorHash hash) pure mnext
        ifM (not <$> isMainBlockHeader next)
            (pure (next, acc))
            (do lsi <- getLocalSlotIndex next
                loop (SlotIndexHash lsi next : acc) next
                )

    errorHash hash =
        CEForwardLink "getHeaderHashesForEpoch" hash


getLocalSlotIndex :: MonadDBRead m => HeaderHash -> ExceptT ConsolidateError m LocalSlotIndex
getLocalSlotIndex hash = do
    meos <- getHeaderEpochOrSlot hash
    case meos of
        Nothing -> throwE $ CEEoSLookupFailed "getLocalSlotIndex" hash
        Just eos ->
            case unEpochOrSlot eos of
                Left _ -> throwE $ CEExcpectedMain "getLocalSlotIndex" hash
                Right sid -> pure $ siSlot sid

isMainBlockHeader :: MonadDBRead m => HeaderHash -> m Bool
isMainBlockHeader hh =
    maybe False (isRight . unEpochOrSlot) <$> getHeaderEpochOrSlot hh

-- -------------------------------------------------------------------------------------------------

getHeaderEpochOrSlot :: MonadDBRead m => HeaderHash -> m (Maybe EpochOrSlot)
getHeaderEpochOrSlot =
    fmap2 getEpochOrSlot . getHeader

getTipEpoch :: MonadDBRead m => m EpochIndex
getTipEpoch = do
    getBlockHeaderEpoch =<< fmap blockHeaderHash getTipHeader


getBlockHeaderEpoch :: MonadDBRead m => HeaderHash -> m EpochIndex
getBlockHeaderEpoch hhash = do
    meos <- getHeaderEpochOrSlot hhash
    case meos of
        Nothing -> error "getBlockHeaderEpoch: Nothing"
        Just eos ->
            case unEpochOrSlot eos of
                Left eid  -> pure eid
                Right sid -> pure $ siEpoch sid


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
