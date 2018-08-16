{-# LANGUAGE RankNTypes #-}

-- | Configuration for the txp package.

module Pos.Chain.Txp.Configuration
       ( TxpConfiguration(..)
       , memPoolLimitTx
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object,
                     (.:), (.=))
import           Data.Aeson.Types (typeMismatch)
import           Pos.Core (Address)

-- | Delegation configruation part.
data TxpConfiguration = TxpConfiguration
    { -- | Limit on the number of transactions that can be stored in
      -- the mem pool.
      ccMemPoolLimitTx      :: !Int

      -- | Set of source address which are asset-locked. Transactions which
      -- use these addresses as transaction inputs will be silently dropped.
    , tcAssetLockedSrcAddrs :: !(Set Address)
    } deriving (Eq,Show,Generic)

instance ToJSON TxpConfiguration where
    toJSON tc = object
        [ "memPoolLimitTx"       .= (ccMemPoolLimitTx tc)
        , "assetLockedSrcAddrs"  .= (tcAssetLockedSrcAddrs tc)
        ]

instance FromJSON TxpConfiguration where
    parseJSON (Object o) = do
        mplt <- o .: "memPoolLimitTx"
        asla <- o .: "assetLockedSrcAddrs"
        pure (TxpConfiguration mplt asla)
    parseJSON invalid = typeMismatch "TxpConfiguration" invalid

----------------------------------------------------------------------------
-- Constants
----------------------------------------------------------------------------

-- | Limint on the number of transactions that can be stored in
-- the mem pool.
memPoolLimitTx :: Integral i => TxpConfiguration -> i
memPoolLimitTx txpConfig = fromIntegral . ccMemPoolLimitTx $ txpConfig