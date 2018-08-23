-- | Dummy values used in tests (replacing `configuration.yaml`)

module Test.Pos.Core.Dummy
       ( dummyNetworkMagic
       , dummyRequiresNetworkMagic
       ) where

import           Pos.Core.Genesis (RequiresNetworkMagic (..))
import           Pos.Core.NetworkMagic (NetworkMagic (..), makeNetworkMagic)

import           Test.Pos.Crypto.Dummy (dummyProtocolMagic)

dummyRequiresNetworkMagic :: RequiresNetworkMagic
dummyRequiresNetworkMagic = NMMustBeNothing

dummyNetworkMagic :: NetworkMagic
dummyNetworkMagic = makeNetworkMagic dummyRequiresNetworkMagic
                                     dummyProtocolMagic
