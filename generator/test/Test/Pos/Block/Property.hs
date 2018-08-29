{-# LANGUAGE RankNTypes #-}

-- | Helpers for 'BlockProperty'.

module Test.Pos.Block.Property
       ( blockPropertySpec
       ) where

import           Universum

import           Test.Hspec (Spec)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck.Property (Testable)

import           Pos.Chain.Delegation (HasDlgConfiguration)
import           Pos.Core (HasConfiguration)
import           Pos.Core.NetworkMagic (NetworkMagic)
import           Pos.Crypto (ProtocolMagic)

import           Test.Pos.Block.Logic.Mode (BlockProperty,
                     blockPropertyTestable)

-- | Specialized version of 'prop' function from 'hspec'.
blockPropertySpec ::
       (HasDlgConfiguration, Testable a)
    => ProtocolMagic
    -> NetworkMagic
    -> String
    -> (HasConfiguration => BlockProperty a)
    -> Spec
blockPropertySpec pm nm description bp =
    prop description (blockPropertyTestable pm nm bp)
