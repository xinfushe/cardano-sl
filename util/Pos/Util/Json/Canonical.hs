{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Canonical encoding of 'GenesisData'.

module Pos.Util.Json.Canonical
       (
       ) where

import           Universum

import           Text.JSON.Canonical (FromJSON (..), JSValue (..), ToJSON (..),
                                      ReportSchemaErrors, expectedButGotValue)


instance Monad m => ToJSON m Int32 where
    toJSON = pure . JSNum . fromIntegral

instance (ReportSchemaErrors m) => FromJSON m Int32 where
    fromJSON (JSNum i) = pure . fromIntegral $ i
    fromJSON val       = expectedButGotValue "Int32" val
