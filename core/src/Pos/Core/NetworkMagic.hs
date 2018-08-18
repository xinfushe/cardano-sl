module Pos.Core.NetworkMagic
       ( NetworkMagic (..)
       , RequiresNetworkMagic (..)
       , makeNetworkMagic
       ) where

import           Universum

import qualified Data.Aeson as A
import           Data.Bits (shift)
import           Data.SafeCopy (SafeCopy (..), contain, safeGet, safePut)
import           Data.Serialize (getWord8, putWord8)
import           Text.JSON.Canonical (FromJSON (..), JSValue (..),
                     ReportSchemaErrors, ToJSON (..), expected)


import           Pos.Crypto.Configuration (ProtocolMagic (..))
import           Pos.Util.Util (cerealError, toAesonError)


--------------------------------------------------------------------------------
-- RequiresNetworkMagic
--------------------------------------------------------------------------------

-- | Bool-isomorphic flag indicating whether we're on testnet
-- or mainnet/staging.
data RequiresNetworkMagic
    = NMMustBeNothing
    | NMMustBeJust
    deriving (Show, Eq)

-- Aeson JSON instances
-- N.B @RequiresNetworkMagic@'s ToJSON & FromJSON instances do not round-trip.
-- They should only be used from a parent instance which handles the
-- `requiresNetworkMagic` key.
instance A.ToJSON RequiresNetworkMagic where
    toJSON NMMustBeNothing = A.String "NMMustBeNothing"
    toJSON NMMustBeJust    = A.String "NMMustBeJust"

instance A.FromJSON RequiresNetworkMagic where
    parseJSON = A.withText "requiresNetworkMagic" $ toAesonError . \case
        "NMMustBeNothing" -> Right NMMustBeNothing
        "NMMustBeJust"    -> Right NMMustBeJust
        other   -> Left ("invalid value " <> show other <>
                         ", acceptable values are NMMustBeNothing | NMMustBeJust")

-- Canonical JSON instances
instance Monad m => ToJSON m RequiresNetworkMagic where
    toJSON NMMustBeNothing = pure (JSString "NMMustBeNothing")
    toJSON NMMustBeJust    = pure (JSString "NMMustBeJust")

instance ReportSchemaErrors m => FromJSON m RequiresNetworkMagic where
    fromJSON = \case
        (JSString "NMMustBeNothing") -> pure NMMustBeNothing
        (JSString    "NMMustBeJust") -> pure NMMustBeJust
        other                        ->
            expected "NMMustBeNothing | NMMustBeJust" (Just (show other))


--------------------------------------------------------------------------------
-- NetworkMagic
--------------------------------------------------------------------------------

data NetworkMagic
    = NMNothing
    | NMJust !Word8
    deriving (Show, Eq, Ord, Generic)

instance NFData NetworkMagic

instance SafeCopy NetworkMagic where
    getCopy = contain $ getWord8 >>= \case
        0 -> pure NMNothing
        1 -> NMJust <$> safeGet
        t -> cerealError $ "getCopy@NetworkMagic: couldn't read tag: " <> show t
    putCopy NMNothing  = contain $ putWord8 0
    putCopy (NMJust x) = contain $ putWord8 1 >> safePut x

makeNetworkMagic :: RequiresNetworkMagic -> ProtocolMagic -> NetworkMagic
makeNetworkMagic rnm pm = case rnm of
    NMMustBeNothing -> NMNothing
    NMMustBeJust    -> NMJust (convert (fromIntegral (getProtocolMagic pm)))
  where
    convert :: Word32 -> Word8
    convert w = let b1 = fromIntegral $ shift        w     (-24)
                    b2 = fromIntegral $ shift (shift w  8) (-24)
                    b3 = fromIntegral $ shift (shift w 16) (-24)
                    b4 = fromIntegral $ shift (shift w 24) (-24)
                 in b1 + b2 + b3 + b4
