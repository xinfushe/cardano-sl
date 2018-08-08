module Pos.DB.Epoch.Index.Naive where

import           Universum

import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List
import qualified Prelude

import           Pos.Core (LocalSlotIndex (..))

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
    } deriving Eq

data SlotIndexOffset = SlotIndexOffset
    { sioSlotIndex :: !Word16
    , sioOffset    :: !Word64
    }

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
