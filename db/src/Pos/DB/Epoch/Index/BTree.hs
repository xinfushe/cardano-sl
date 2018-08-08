module Pos.DB.Epoch.Index.BTree
       ( writeEpochIndex
       , getEpochBlockOffset
       , SlotIndexOffset (..)
       ) where

import           Universum

import           BTree (BLeaf (..), fromOrderedToFile, lookup, open)
import           Pipes (each)

import           Pos.Core (LocalSlotIndex (..))
import           Pos.DB.Epoch.Index.Naive (SlotIndexOffset (..))

toLeaf :: SlotIndexOffset -> BLeaf Word16 Word64
toLeaf (SlotIndexOffset si offset) = BLeaf si offset

writeEpochIndex :: FilePath -> [SlotIndexOffset] -> IO ()
writeEpochIndex path =
    fromOrderedToFile order blocksPerEpoch path . each . fmap toLeaf
  where
    -- order chosen through benchmarking
    order          = 14
    blocksPerEpoch = 220000

getEpochBlockOffset :: FilePath -> LocalSlotIndex -> IO (Maybe Word64)
getEpochBlockOffset fpath (UnsafeLocalSlotIndex k) =
    open fpath >>= pure . either (const Nothing) (flip lookup k)
