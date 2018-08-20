module Test.Pos.Binary.CommunicationSpec
    ( spec )
    where

import           Universum

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as Set
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck.Monadic (assert)

import           Pos.Binary.Class (decodeFull, serialize')
import           Pos.Binary.Communication (serializeMsgSerializedBlock)
import           Pos.Chain.Txp (TxpConfiguration (..))
import           Pos.Core.NetworkMagic (RequiresNetworkMagic (..))
import           Pos.DB.Class (Serialized (..))
import           Pos.Network.Block.Types (MsgBlock (..),
                     MsgSerializedBlock (..))
import           Pos.Util.CompileInfo (withCompileInfo)

import           Test.Pos.Block.Logic.Mode (blockPropertyTestable)
import           Test.Pos.Block.Logic.Util (EnableTxPayload (..),
                     InplaceDB (..), bpGenBlock)
import           Test.Pos.Configuration (HasStaticConfigurations,
                     withStaticConfigurations)
import           Test.Pos.Crypto.Dummy (dummyProtocolMagic)

-- |
-- The binary encoding of `MsgSerializedBlock` using `serializeMsgSerializedBlock`
-- should be the same as the binary encoding of `MsgBlock`.
serializeMsgSerializedBlockSpec
    :: (HasStaticConfigurations) => Spec
serializeMsgSerializedBlockSpec = do
    go NMMustBeNothing
    go NMMustBeJust
  where
    go :: RequiresNetworkMagic -> Spec
    go rnm = do
        let addNetworkMagicBlurb msg = msg <> " (requiresNetworkMagic=" <> show rnm <> ")"
        let desc = addNetworkMagicBlurb "serializeMsgSerializedBlock for MsgSerializedBlock should \
                                        \create the same ByteString as serialize' for MsgBlock"
        let descNoBlock = addNetworkMagicBlurb "serializeMsgSerializedBlock MsgNoSerializedBlock should \
                                               \create the same ByteString as serialize' for MsgNoBlock"
        prop desc $ blockPropertyTestable rnm $ do
            (block, _) <- bpGenBlock dummyProtocolMagic (TxpConfiguration 200 Set.empty) (EnableTxPayload True) (InplaceDB True)
            let sb = Serialized $ serialize' block
            assert $ serializeMsgSerializedBlock (MsgSerializedBlock sb) == serialize' (MsgBlock block)
        prop descNoBlock $ blockPropertyTestable rnm $ do
            let msg :: MsgSerializedBlock
                msg = MsgNoSerializedBlock "no block"
                msg' :: MsgBlock
                msg' = MsgNoBlock "no block"
            assert $ serializeMsgSerializedBlock msg == serialize' msg'


-- |
-- Deserialization of a serialized `MsgSerializedBlock` (with
-- `serializeMsgSerializedBlock`) should give back the original block.
deserializeSerilizedMsgSerializedBlockSpec
    :: (HasStaticConfigurations) => Spec
deserializeSerilizedMsgSerializedBlockSpec = do
    prop desc $ blockPropertyTestable $ do
        (block, _) <- bpGenBlock dummyProtocolMagic (TxpConfiguration 200 Set.empty) (EnableTxPayload True) (InplaceDB True)
        let sb = Serialized $ serialize' block
        let msg :: Either Text MsgBlock
            msg = decodeFull . BSL.fromStrict . serializeMsgSerializedBlock $ MsgSerializedBlock sb
        assert $ msg == Right (MsgBlock block)
    prop descNoBlock $ blockPropertyTestable $ do
        let msg :: MsgSerializedBlock
            msg = MsgNoSerializedBlock "no block"
        assert $ (decodeFull . BSL.fromStrict . serializeMsgSerializedBlock $ msg) == Right (MsgNoBlock "no block")
    where
    desc = "deserialization of a serialized MsgSerializedBlock message should give back corresponding MsgBlock"
    descNoBlock = "deserialization of a serialized MsgNoSerializedBlock message should give back corresponding MsgNoBlock"

spec :: Spec
spec = withStaticConfigurations $ \_ _ -> withCompileInfo $
    describe "Pos.Binary.Communication" $ do
        describe "serializeMsgSerializedBlock" serializeMsgSerializedBlockSpec
        describe "decode is left inverse of serializeMsgSerializedBlock" deserializeSerilizedMsgSerializedBlockSpec
