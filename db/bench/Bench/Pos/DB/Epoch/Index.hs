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
import           Pos.DB.Epoch.Index.Naive as Naive

import           Test.Pos.Core.Gen (genLocalSlotIndex)

protocolConstants :: ProtocolConstants
protocolConstants = ProtocolConstants
    { pcK         = 2
    , pcVssMinTTL = VssMinTTL 2
    , pcVssMaxTTL = VssMaxTTL 6
    }

genIndices :: IO [LocalSlotIndex]
genIndices = replicateM 10 $ sample $ genLocalSlotIndex protocolConstants

mkIndexBench
    :: String
    -> (FilePath -> LocalSlotIndex -> IO (Maybe Int64))
    -> [LocalSlotIndex]
    -> Benchmark
mkIndexBench mod f =
    bench (mod <> "Index Bench") . nfIO . traverse (f "epochIndex")

naiveBench :: [LocalSlotIndex] -> Benchmark
naiveBench = mkIndexBench "Naive " Naive.getEpochBlockOffset

runBenchmark :: IO ()
runBenchmark = do
    writeEpochIndex "epochIndex" exampleIndex
    indices <- genIndices
    defaultMainWith (defaultConfig { reportFile = Just "naiveIndex.html" })
                    [naiveBench indices]
