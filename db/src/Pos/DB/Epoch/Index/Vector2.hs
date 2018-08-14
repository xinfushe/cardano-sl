module Pos.DB.Epoch.Index.Vector2
       ( writeEpochIndex
       , getEpochBlockOffset
       ) where

import           Universum hiding (Vector)

import           Data.Vector.Storable (Storable, Vector)
import qualified Data.Vector.Storable as SVector

import           Foreign.ForeignPtr (mallocForeignPtrArray0, withForeignPtr)
import           Foreign.Storable (sizeOf)

-- Need this so it builds on Linux and Windows!!!!
import           GHC.IO.Handle (hFileSize)

import           System.IO (IOMode (..), hGetBuf, hPutBuf, withBinaryFile)

import           Pos.Core (LocalSlotIndex (..))
import           Pos.DB.Epoch.Index.Naive (SlotIndexOffset (..))


writeEpochIndex :: FilePath -> Vector SlotIndexOffset -> IO ()
writeEpochIndex = writeStorableVector

getEpochBlockOffset :: FilePath -> LocalSlotIndex -> IO (Maybe Word64)
getEpochBlockOffset fpath (UnsafeLocalSlotIndex k) = do
    v <- readStorableVector fpath (SlotIndexOffset 0 0)
    pure $ searchLocalSlotIndex v k

searchLocalSlotIndex :: Vector SlotIndexOffset -> Word16 -> Maybe Word64
searchLocalSlotIndex vec k
    | vecLen >= epochSlots = Nothing -- Should not happen.
    | k <= diffLen = go 0 (fromIntegral k)
    | otherwise =  go (fromIntegral $ k - diffLen) (fromIntegral k)
  where
    -- Need to get this from ProtocolConstants
    epochSlots :: Word16
    epochSlots = 21600

    vecLen :: Word16
    vecLen = fromIntegral $ SVector.length vec

    diffLen :: Word16
    diffLen = epochSlots - vecLen

    go :: Int -> Int -> Maybe Word64
    go l u =
        let siol = SVector.unsafeIndex vec l
            siou = SVector.unsafeIndex vec u
        in if
            | sioSlotIndex siol == k -> Just $ sioOffset siol
            | sioSlotIndex siou == k -> Just $ sioOffset siou
            | l  + 1 >= u -> Nothing -- Not found
            | otherwise -> do
                let m = (l + u) `div` 2
                    siom = SVector.unsafeIndex vec m
                if sioSlotIndex siom <= k
                    then go m u
                    else go l m

-- -----------------------------------------------------------------------------

writeStorableVector :: Storable a => FilePath -> Vector a -> IO ()
writeStorableVector fpath vec =
    withBinaryFile fpath WriteMode $ \ hdl -> do
        let elemSize = sizeOf $ SVector.head vec
        SVector.unsafeWith vec $ \ptr ->
            hPutBuf hdl ptr (elemSize * SVector.length vec)

readStorableVector :: Storable a => FilePath -> a -> IO (Vector a)
readStorableVector fpath velem =
    withBinaryFile fpath ReadMode $ \ hdl -> do
        fsize <- fromIntegral <$> hFileSize hdl
        let elemSize = sizeOf velem
            elemCount = fsize `div` elemSize
        fptr <- mallocForeignPtrArray0 elemCount
        readCount <- withForeignPtr fptr $ \ptr ->
                        hGetBuf hdl ptr (elemSize * elemCount)
        pure $ SVector.unsafeFromForeignPtr0 fptr (readCount`div` elemSize)
