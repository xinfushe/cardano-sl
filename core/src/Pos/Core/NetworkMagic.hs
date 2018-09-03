module Pos.Core.NetworkMagic
       ( NetworkMagic (..)
       , makeNetworkMagic
       ) where

import           Universum

import           Data.Bits (shift)
import           Data.SafeCopy (SafeCopy (..), contain, safeGet, safePut)
import           Data.Serialize (getWord8, putWord8)
import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, (%))

import           Pos.Crypto.Configuration (ProtocolMagic (..), RequiresNetworkMagic (..),
                                           getProtocolMagic)
import           Pos.Util.Util (cerealError)


--------------------------------------------------------------------------------
-- NetworkMagic
--------------------------------------------------------------------------------

data NetworkMagic
    = NMNothing
    | NMJust !Word8
    deriving (Show, Eq, Ord, Generic)

instance NFData NetworkMagic

instance Buildable NetworkMagic where
    build NMNothing  = "NMNothing"
    build (NMJust n) = bprint ("NMJust ("%build%")") n

instance SafeCopy NetworkMagic where
    getCopy = contain $ getWord8 >>= \case
        0 -> pure NMNothing
        1 -> NMJust <$> safeGet
        t -> cerealError $ "getCopy@NetworkMagic: couldn't read tag: " <> show t
    putCopy NMNothing  = contain $ putWord8 0
    putCopy (NMJust x) = contain $ putWord8 1 >> safePut x

makeNetworkMagic :: ProtocolMagic -> NetworkMagic
makeNetworkMagic pm = case getRequiresNetworkMagic pm of
    NMMustBeNothing -> NMNothing
    NMMustBeJust    -> NMJust (convert (fromIntegral ident))
  where
    ident = getProtocolMagic pm
    --
    convert :: Word32 -> Word8
    convert w = let b1 = fromIntegral $ shift        w     (-24)
                    b2 = fromIntegral $ shift (shift w  8) (-24)
                    b3 = fromIntegral $ shift (shift w 16) (-24)
                    b4 = fromIntegral $ shift (shift w 24) (-24)
                 in b1 + b2 + b3 + b4
