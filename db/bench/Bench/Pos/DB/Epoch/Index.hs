module Bench.Pos.DB.Epoch.Index
       ( runBenchmark
       ) where

import           Universum

import           Criterion.Main (Benchmark, bench, bgroup, defaultConfig,
                     defaultMainWith, env, nfIO)
import           Criterion.Types (Config (..))
import           Hedgehog.Gen (sample)

import           Pos.Core (LocalSlotIndex (..), ProtocolConstants (..),
                     VssMaxTTL (..), VssMinTTL (..))
import           Pos.DB.Epoch.Index.BTree as BTree
import           Pos.DB.Epoch.Index.Naive as Naive

import           Test.Pos.Core.Gen (genLocalSlotIndex)

exampleIndex :: [SlotIndexLength]
exampleIndex =
    [SlotIndexLength 0 10, SlotIndexLength 1 14, SlotIndexLength 10 150]

exampleOffsetIndex :: [BTree.SlotIndexOffset]
exampleOffsetIndex =
    [ BTree.SlotIndexOffset 0  0
    , BTree.SlotIndexOffset 1  10
    , BTree.SlotIndexOffset 10 24
    ]

protocolConstants :: ProtocolConstants
protocolConstants = ProtocolConstants
    { pcK         = 2
    , pcVssMinTTL = VssMinTTL 2
    , pcVssMaxTTL = VssMaxTTL 6
    }

genIndices :: IO [LocalSlotIndex]
genIndices = replicateM 50 $ sample $ genLocalSlotIndex protocolConstants

mkIndexBench
    :: String
    -> (LocalSlotIndex -> IO (Maybe Word64))
    -> [LocalSlotIndex]
    -> Benchmark
mkIndexBench mod f = bench (mod <> "Index Bench") . nfIO . traverse f

naiveBench :: [LocalSlotIndex] -> Benchmark
naiveBench = mkIndexBench "Naive " (Naive.getEpochBlockOffset "naiveEpochIndex")

btreeBench :: [LocalSlotIndex] -> Benchmark
btreeBench = mkIndexBench "BTree " (BTree.getEpochBlockOffset "btreeEpochIndex")

runBenchmark :: IO ()
runBenchmark = do
    Naive.writeEpochIndex "naiveEpochIndex" exampleIndex
    BTree.writeEpochIndex "btreeEpochIndex" exampleOffsetIndex
    indices <- genIndices
    defaultMainWith (defaultConfig { reportFile = Just "naiveIndex.html" })
                    [naiveBench indices, btreeBench indices]
