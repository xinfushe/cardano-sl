module Pos.DB.Epoch.Index.Vector
       ( writeEpochIndex
       , getEpochBlockOffset
       ) where

import           Universum

import           Data.Binary (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Vector.Binary ()
import qualified Data.Vector.Unboxed as V

import           Pos.Core (LocalSlotIndex (..))
import           Pos.DB.Epoch.Index (SlotIndexOffset (..))
import           Pos.DB.Epoch.Index.Naive ()

writeEpochIndex :: FilePath -> V.Vector SlotIndexOffset -> IO ()
writeEpochIndex path = BL.writeFile path . encode

getEpochBlockOffset :: FilePath -> LocalSlotIndex -> IO (Maybe Word64)
getEpochBlockOffset fpath (UnsafeLocalSlotIndex k) = do
    v <- decode . BL.fromStrict <$> BS.readFile fpath
    -- let sliceStart =
    --         if fromIntegral k > missing then fromIntegral k - missing else 0
    --     searchVector = V.slice sliceStart (missing + 1) v
    pure $ sioOffset <$> V.find ((==) k . sioSlotIndex) v -- searchVector
    -- where missing = 4
