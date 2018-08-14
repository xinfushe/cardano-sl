{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Pos.DB.Epoch.Index.Naive where

import           Universum

import           Data.Binary (Binary (..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List
import           Data.Vector.Unboxed.Deriving (derivingUnbox)
import           Foreign.Ptr (castPtr)
import           Foreign.Storable (Storable (..))
import qualified Prelude

import           Pos.Core (LocalSlotIndex (..))
import           Pos.DB.Epoch.Index (SlotIndexOffset (..))

--------------------------------------------------------------------------------
-- Naive Implementation
--------------------------------------------------------------------------------

epochFileHeader :: ByteString
epochFileHeader = ""

 -- Use sized types here because we would like store these as binary in the
-- epoch index file.
data SlotIndexLength = SlotIndexLength
    { silSlotIndex :: !Word16
    , silLength    :: !Word32 -- Length of the block for the given slot index (up to 2 MB)
    } deriving (Eq, Generic, Show)

instance Binary SlotIndexLength

derivingUnbox "SlotIndexOffset"
    [t| SlotIndexOffset -> (Word16, Word64) |]
    [| \ (SlotIndexOffset i o) -> (i, o) |]
    [| uncurry SlotIndexOffset |]

instance Storable SlotIndexOffset where
    sizeOf _ = 10
    alignment _ = 10
    peekByteOff ptr idx =
        SlotIndexOffset
            <$> peekByteOff (castPtr ptr) (10 * idx)
            <*> peekByteOff (castPtr ptr) (10 * idx + 2)
    pokeByteOff ptr idx sio = do
        pokeByteOff (castPtr ptr) (idx * 10) $ sioSlotIndex sio
        pokeByteOff (castPtr ptr) (idx * 10 + 2) $ sioOffset sio


writeEpochIndex :: FilePath -> [SlotIndexLength] -> IO ()
writeEpochIndex fpath xs =
    withFile fpath WriteMode $ \ hdl ->
        BS.hPutStrLn hdl $ show (map (\x -> (silSlotIndex x, silLength x)) xs)

readEpochIndex :: FilePath -> IO [SlotIndexLength]
readEpochIndex fpath =
    withFile fpath ReadMode $ \ hdl ->
        map (uncurry SlotIndexLength) . Prelude.read . BS.unpack
            <$> BS.hGetContents hdl

epochIndexToOffset :: [SlotIndexLength] -> [SlotIndexOffset]
epochIndexToOffset =
    snd . mapAccumL convert (fromIntegral $ BS.length epochFileHeader)
  where
    convert :: Word64 -> SlotIndexLength -> (Word64, SlotIndexOffset)
    convert offset (SlotIndexLength a b) =
        (offset + fromIntegral b, SlotIndexOffset a offset)

readEpochOffsets :: FilePath -> IO [SlotIndexOffset]
readEpochOffsets fpath =
    epochIndexToOffset <$> readEpochIndex fpath

findEpochBlockOffset :: LocalSlotIndex -> [SlotIndexOffset] -> Maybe Word64
findEpochBlockOffset (UnsafeLocalSlotIndex lsi) xs =
    sioOffset <$> List.find (\x -> sioSlotIndex x == lsi) xs

getEpochBlockOffset :: FilePath -> LocalSlotIndex -> IO (Maybe Word64)
getEpochBlockOffset fpath lsi =
    findEpochBlockOffset lsi <$> readEpochOffsets fpath
