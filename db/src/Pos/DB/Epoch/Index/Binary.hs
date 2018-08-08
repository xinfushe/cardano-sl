module Pos.DB.Epoch.Index.Binary
       ( writeEpochIndex
       , getEpochBlockOffset
       ) where

import           Universum

import           Data.Binary (decodeFile, encodeFile)

import           Pos.Core (LocalSlotIndex (..))
import           Pos.DB.Epoch.Index.Naive (SlotIndexLength (..),
                     epochIndexToOffset, findEpochBlockOffset)

writeEpochIndex :: FilePath -> [SlotIndexLength] -> IO ()
writeEpochIndex = encodeFile

readEpochIndex :: FilePath -> IO [SlotIndexLength]
readEpochIndex = decodeFile

getEpochBlockOffset :: FilePath -> LocalSlotIndex -> IO (Maybe Word64)
getEpochBlockOffset fpath lsi =
    findEpochBlockOffset lsi . epochIndexToOffset <$> readEpochIndex fpath
