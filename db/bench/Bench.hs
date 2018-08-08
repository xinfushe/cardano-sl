module Main
       ( main
       ) where

import           Universum

import qualified Bench.Pos.DB.Epoch.Index as NaiveIndex

main :: IO ()
main = do
  NaiveIndex.runBenchmark
