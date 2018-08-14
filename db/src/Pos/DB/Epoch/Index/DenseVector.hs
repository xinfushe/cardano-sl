module Pos.DB.Epoch.Index.DenseVector
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

writeEpochIndex :: FilePath -> [SlotIndexOffset] -> IO ()
writeEpochIndex fpath = writeStorableVector fpath . SVector.fromList . padIndex

-- | Pad a list of @SlotIndexOffset@s ordered by @LocalSlotIndex@
padIndex :: [SlotIndexOffset] -> [SlotIndexOffset]
padIndex = go (flip SlotIndexOffset maxBound <$> [0 .. 21599])
  where
    go [] _  = []
    go xs [] = xs
    go (x : xs) (y : ys) | sioSlotIndex x == sioSlotIndex y = y : go xs ys
                         | otherwise                        = x : go xs (y : ys)

getEpochBlockOffset :: FilePath -> LocalSlotIndex -> IO (Maybe Word64)
getEpochBlockOffset fpath (UnsafeLocalSlotIndex k) = do
    v <- readStorableVector fpath (0 :: Word64)
    pure $ searchLocalSlotIndex v k

searchLocalSlotIndex :: Vector Word64 -> Word16 -> Maybe Word64
searchLocalSlotIndex v k = if val == maxBound then Nothing else Just val
    where val = SVector.unsafeIndex v (fromIntegral k)

--------------------------------------------------------------------------------

writeStorableVector :: Storable a => FilePath -> Vector a -> IO ()
writeStorableVector fpath vec = withBinaryFile fpath WriteMode $ \hdl -> do
    let elemSize = sizeOf $ SVector.head vec
    SVector.unsafeWith vec
        $ \ptr -> hPutBuf hdl ptr (elemSize * SVector.length vec)

readStorableVector :: Storable a => FilePath -> a -> IO (Vector a)
readStorableVector fpath velem = withBinaryFile fpath ReadMode $ \hdl -> do
    fsize <- fromIntegral <$> hFileSize hdl
    let elemSize  = sizeOf velem
        elemCount = fsize `div` elemSize
    fptr      <- mallocForeignPtrArray0 elemCount
    readCount <- withForeignPtr fptr
        $ \ptr -> hGetBuf hdl ptr (elemSize * elemCount)
    pure $ SVector.unsafeFromForeignPtr0 fptr (readCount `div` elemSize)
