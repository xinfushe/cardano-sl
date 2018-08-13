module Bench.Pos.DB.Epoch.Index
       ( runBenchmark
       ) where

import           Universum

import           Criterion.Main (Benchmark, bench, bgroup, defaultConfig,
                     defaultMainWith, env, nfIO)
import           Criterion.Types (Config (..))
import qualified Data.Vector.Unboxed as V
import           Hedgehog.Gen (sample)

import           Pos.Core (LocalSlotIndex (..), ProtocolConstants (..),
                     VssMaxTTL (..), VssMinTTL (..))
import           Pos.DB.Epoch.Index.Binary as Binary
import           Pos.DB.Epoch.Index.BTree as BTree
import           Pos.DB.Epoch.Index.Naive as Naive
import           Pos.DB.Epoch.Index.Vector as Vector

import           Test.Pos.Core.Gen (genLocalSlotIndex)

runBenchmark :: IO ()
runBenchmark = do
    exampleIndex <- Naive.readEpochIndex "bench/naive.index"
    indices      <- genIndices
    defaultMainWith
        (defaultConfig { reportFile = Just "naiveIndex.html" })
        [ naiveBench  exampleIndex indices
        , binaryBench exampleIndex indices
        , btreeBench exampleIndex indices
        , vectorBench exampleIndex indices
        ]

genIndices :: IO [LocalSlotIndex]
genIndices = replicateM 1000 $ sample $ genLocalSlotIndex protocolConstants

protocolConstants :: ProtocolConstants
protocolConstants = ProtocolConstants
    { pcK         = 2
    , pcVssMinTTL = VssMinTTL 2
    , pcVssMaxTTL = VssMaxTTL 6
    }

--------------------------------------------------------------------------------
-- Benchmarks
--------------------------------------------------------------------------------

naiveBench :: [SlotIndexLength] -> [LocalSlotIndex] -> Benchmark
naiveBench index = mkIndexBench "Naive"
                                (Naive.writeEpochIndex "naive.index" index)
                                (Naive.getEpochBlockOffset "naive.index")

binaryBench :: [SlotIndexLength] -> [LocalSlotIndex] -> Benchmark
binaryBench index = mkIndexBench
    "Binary"
    (Binary.writeEpochIndex "binary.index" $ epochIndexToOffset index)
    (Binary.getEpochBlockOffset "binary.index")

btreeBench :: [SlotIndexLength] -> [LocalSlotIndex] -> Benchmark
btreeBench index = mkIndexBench
    "BTree"
    (BTree.writeEpochIndex indexFile $ epochIndexToOffset index)
    (BTree.getEpochBlockOffset indexFile)
    where indexFile = "btree.index"

vectorBench :: [SlotIndexLength] -> [LocalSlotIndex] -> Benchmark
vectorBench index = mkIndexBench
    "Vector"
    (Vector.writeEpochIndex "vector.index" $ V.fromList $ epochIndexToOffset
        index
    )
    (Vector.getEpochBlockOffset "vector.index")

mkIndexBench
    :: String
    -> IO ()
    -> (LocalSlotIndex -> IO (Maybe Word64))
    -> [LocalSlotIndex]
    -> Benchmark
mkIndexBench mod write query queries = bgroup
    (mod <> " Index")
    [ bench "Writing" . nfIO $ write
    , bench "Querying" . nfIO $ traverse query queries
    ]
