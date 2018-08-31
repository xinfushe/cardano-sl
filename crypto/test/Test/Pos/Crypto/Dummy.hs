-- | Dummy values used in tests (replacing `configuration.yaml`)

module Test.Pos.Crypto.Dummy
       ( dummyProtocolMagic
       ) where

import           Pos.Crypto (ProtocolMagic (..), ProtocolMagicId (..),
                     RequiresNetworkMagic (..))

dummyProtocolMagic :: ProtocolMagic
dummyProtocolMagic = ProtocolMagic (ProtocolMagicId 55550001) NMMustBeNothing
