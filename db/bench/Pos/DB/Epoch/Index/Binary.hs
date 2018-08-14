{-# LANGUAGE DeriveGeneric #-}

module Pos.DB.Epoch.Index.Binary
       ( writeEpochIndex
       , getEpochBlockOffset
       , getSlotIndexOffsetN
       , getSlotIndicesOffsetN
       , hGetIndexHeader
       ) where

import           Universum

import           Data.Binary (Binary, decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import           System.IO (IOMode (..), SeekMode (..), hSeek, withBinaryFile)

import           Pos.Core (LocalSlotIndex (..))
import           Pos.DB.Epoch.Index (SlotIndexOffset (..))
import           Pos.DB.Epoch.Index.Naive (findEpochBlockOffset)

{-

We want to write a list of [SlotIndexOffsets] to disk. The entries for some
@LocalSlotIndex@s might be missing. If there are X missing values and N total
slots, then we only have to search between N - X and N for that entry.

-}

newtype IndexHeader = IndexHeader
    { getIndexHeader :: Word16
    } deriving (Generic, Show)

instance Binary IndexHeader

headerLength :: Int64
headerLength = BL.length $ encode (IndexHeader 0 :: IndexHeader)

hGetIndexHeader :: Handle -> IO IndexHeader
hGetIndexHeader h = do
    headerBytes <- BL.hGet h (fromIntegral headerLength)
    pure $ decode headerBytes

writeEpochIndex :: FilePath -> [SlotIndexOffset] -> IO ()
writeEpochIndex path index = do
    let missing       = fromIntegral $ 21600 - length index
        headerBuilder = B.lazyByteString $ encode (missing :: Word16)
        bodyBuilder   = foldMap (B.lazyByteString . encode) index
    withBinaryFile path WriteMode
        .  flip B.hPutBuilder
        $  headerBuilder
        <> bodyBuilder

getSlotIndexOffsetN :: FilePath -> LocalSlotIndex -> IO SlotIndexOffset
getSlotIndexOffsetN path (UnsafeLocalSlotIndex i) =
    withBinaryFile path ReadMode $ \h -> do
        hSeek h AbsoluteSeek (fromIntegral $ i * 10 + 2)
        b <- BL.hGet h 10
        pure $ decode b

chunksOfN :: Int -> BS.ByteString -> [BS.ByteString]
chunksOfN n s = go [] s
  where
    go acc s'
        | BS.length s' < 10
        = acc
        | otherwise
        = let (chunk, rest) = BS.splitAt n s' in go (acc ++ [chunk]) rest

getSlotIndicesOffsetN :: FilePath -> LocalSlotIndex -> IO [SlotIndexOffset]
getSlotIndicesOffsetN path (UnsafeLocalSlotIndex i) =
    withBinaryFile path ReadMode $ \h -> do
        header <- getIndexHeader <$> hGetIndexHeader h
        let seekLoc = if i > header then i - header else 0
        hSeek h
              AbsoluteSeek
              (fromIntegral $ seekLoc * 10 + fromIntegral headerLength)
        b <- BS.hGetSome h ((fromIntegral header + 1) * 10)
        let indices = decode . BL.fromStrict <$> chunksOfN 10 b
        pure indices

getEpochBlockOffset :: FilePath -> LocalSlotIndex -> IO (Maybe Word64)
getEpochBlockOffset fpath lsi =
    findEpochBlockOffset lsi <$> getSlotIndicesOffsetN fpath lsi
