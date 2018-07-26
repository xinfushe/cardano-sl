module Util.Aeson
    ( parseJSONP
    ) where

import           Universum

import           Data.Aeson (FromJSON, Result (..), fromJSON)
import           Data.Aeson.Parser (json)
import           Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit.Combinators as C
import           Util.Conduit (parseP)

parseJSONP :: (FromJSON a, Monad m) => ConduitT ByteString a m ()
parseJSONP = parseP json .| C.map (fromResult . fromJSON)
  where
    fromResult :: Result a -> a
    fromResult (Success a) = a
    fromResult (Error e)   = error $ toText e
