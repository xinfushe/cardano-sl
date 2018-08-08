module Pos.DB.Epoch.Index.BTree
       ( writeEpochIndex
       , getEpochBlockOffset
       , SlotIndexOffset (..)
       ) where

import           Universum

import           BTree (BLeaf (..), fromOrderedToFile, lookup, open)
import           Pipes (yield)

import           Pos.Core (LocalSlotIndex (..))

data SlotIndexOffset = SlotIndexOffset
    { sioSlotIndex :: !Word16
    , sioOffset    :: !Word64
    }

toLeaf :: SlotIndexOffset -> BLeaf Word16 Word64
toLeaf (SlotIndexOffset si offset) = BLeaf si offset

writeEpochIndex :: FilePath -> [SlotIndexOffset] -> IO ()
writeEpochIndex path = fromOrderedToFile 10 blocksPerEpoch path
    . traverse (yield . toLeaf)
    where blocksPerEpoch = 10

getEpochBlockOffset :: FilePath -> LocalSlotIndex -> IO (Maybe Word64)
getEpochBlockOffset fpath (UnsafeLocalSlotIndex k) =
    open fpath >>= pure . either (const Nothing) (flip lookup k)
