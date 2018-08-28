{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.Kernel.Restore
    ( restoreWallet
    ) where

import           Universum

import           Control.Concurrent.Async (async, cancel)
import           Control.Concurrent.MVar (modifyMVar_)
import           Data.Acid (update)
import qualified Data.Map.Merge.Strict as M
import qualified Data.Map.Strict as M
import           Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime,
                     getCurrentTime)
import           Formatting (bprint, build, formatToString, (%))
import qualified Formatting.Buildable

import qualified Prelude

import           Cardano.Wallet.API.Types.UnitOfMeasure
import           Cardano.Wallet.Kernel (walletLogMessage)
import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.DB.AcidState (ApplyHistoricalBlock (..),
                     CreateHdWallet (..), RestorationComplete (..),
                     RestoreHdWallet (..))
import           Cardano.Wallet.Kernel.DB.BlockContext
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.HdWallet.Create (CreateHdRootError)
import qualified Cardano.Wallet.Kernel.DB.Spec.Update as Spec
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.Decrypt (WalletDecrCredentialsKey (..),
                     decryptAddress, keyToWalletDecrCredentials)
import           Cardano.Wallet.Kernel.Internal (WalletRestorationInfo (..),
                     cancelRestoration, walletMeta, walletNode,
                     walletRestorationTask, wallets, wriCancel, wriCurrentSlot,
                     wriTargetSlot, wriThroughput)
import           Cardano.Wallet.Kernel.NodeStateAdaptor (Lock, LockContext (..),
                     NodeConstraints, WithNodeState, filterUtxo,
                     getSecurityParameter, getSlotCount, mostRecentMainBlock,
                     withNodeState)
import           Cardano.Wallet.Kernel.PrefilterTx (AddrWithId,
                     PrefilteredBlock, UtxoWithAddrId, WalletKey,
                     prefilterUtxo', toHdAddressId, toPrefilteredUtxo)
import           Cardano.Wallet.Kernel.Types (WalletId (..))
import           Cardano.Wallet.Kernel.Util.Core (utxoBalance)
import           Cardano.Wallet.Kernel.Wallets (createWalletHdRnd)

import           Pos.Chain.Block (Block, Blund, HeaderHash, MainBlock, Undo,
                     headerHash, mainBlockSlot)
import           Pos.Chain.Txp (GenesisUtxo (..), Utxo, genesisUtxo)
import           Pos.Core (BlockCount (..), Coin, SlotId, flattenSlotId, mkCoin,
                     unsafeIntegerToCoin)
import           Pos.Core.Txp (TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Crypto (EncryptedSecretKey)
import           Pos.DB.Block (getFirstGenesisBlockHash, getUndo,
                     resolveForwardLink)
import           Pos.DB.Class (getBlock)
import           Pos.Util.Trace (Severity (Error))

-- | Restore a wallet
--
-- NOTE: The key for the wallet must already have been added to the keystore.
--
-- Scan the node's current UTXO set for any that belong to this wallet. Use them
-- to update the current checkpoint's UTXO set, and return the total 'Coin'
-- value of the UTXO belonging to this wallet. At the same time, kick off a
-- background thread that will asynchronously restore the wallet history.
--
-- Wallet initialization parameters match those of 'createWalletHdRnd'
restoreWallet :: Kernel.PassiveWallet
              -> Bool -- ^ Spending password
              -> HD.WalletName
              -> HD.AssuranceLevel
              -> EncryptedSecretKey
              -> (Blund -> IO (Map HD.HdAccountId PrefilteredBlock, [TxMeta]))
              -> IO (Either CreateHdRootError (HD.HdRoot, Coin))
restoreWallet pw spendingPass name assurance esk prefilter = do
    walletInitInfo <- withNodeState (pw ^. walletNode) $ getWalletInitInfo wkey
    case walletInitInfo of
      WalletCreate utxos -> do
        root <- createWalletHdRnd pw spendingPass name assurance esk $ \root ->
                  Left $ CreateHdWallet root utxos
        return $ fmap (, mkCoin 0) root
      WalletRestore utxos tgt -> do
          -- Create the wallet for restoration, deleting the wallet first if it
          -- already exists.
          mRoot <- createWalletHdRnd pw spendingPass name assurance esk $ \root ->
               Right $ RestoreHdWallet root utxos
          case mRoot of
              Left  err  -> return (Left err)
              Right root -> do
                  -- Start the restoration task.
                  beginRestoration pw wId prefilter root tgt (restart root)

                  -- Return the wallet's current balance.
                  let coins = unsafeIntegerToCoin
                            . utxoBalance
                            . M.unions
                            . M.elems
                            . fmap (\(cur, _gen, _addrs) -> cur)
                            $ utxos
                  return (Right (root, coins))

  where
    restart :: HD.HdRoot -> IO ()
    restart root = do
        walletInitInfo <- withNodeState (pw ^. walletNode) $ getWalletInitInfo wkey
        case walletInitInfo of
            WalletCreate _utxos -> return ()
            WalletRestore _utxos tgt ->
                beginRestoration pw wId prefilter root tgt (restart root)

    wId    = WalletIdHdRnd (HD.eskToHdRootId esk)
    wkey   = (wId, keyToWalletDecrCredentials (KeyForRegular esk))


beginRestoration  :: Kernel.PassiveWallet
                  -> WalletId
                  -> (Blund -> IO (Map HD.HdAccountId PrefilteredBlock, [TxMeta]))
                  -> HD.HdRoot
                  -> (HeaderHash, SlotId)
                  -> IO ()
                  -> IO ()
beginRestoration pw wId prefilter root (tgtTip, tgtSlot) restart = do

    -- Set the wallet's restoration information
    slotCount <- getSlotCount (pw ^. walletNode)
    let restoreInfo = WalletRestorationInfo
                      { _wriCurrentSlot = 0
                      , _wriTargetSlot  = flattenSlotIdExplicit slotCount tgtSlot
                      , _wriThroughput  = MeasuredIn 0
                      , _wriCancel      = return ()
                      , _wriRestart     = restart
                      }
    modifyMVar_ (pw ^. walletRestorationTask) $ \wri -> do
        -- Cancel any other restorations currently running for this wallet.
        whenJust (M.lookup wId wri) cancelRestoration
        -- Register this restoration task with the wallet.
        return (M.insert wId restoreInfo wri)

    -- Begin restoring the wallet history in the background.
    restoreTask <- async $
        -- We are starting this async /from/ a thread that runs in response
        -- to a REST request. Linking the async to that REST request thread
        -- is pointless, because that thread will probably be long gone if
        -- an exception ever happens in the restoration worker. Therefore
        -- we just log any errors.
        catch (restoreWalletHistoryAsync pw
                                         (root ^. HD.hdRootId)
                                         prefilter
                                         (tgtTip, tgtSlot)) $ \(e :: SomeException) ->
              (pw ^. walletLogMessage) Error ("Exception during restoration: " <> show e)

    -- Set up the cancellation action
    updateRestorationInfo pw wId (wriCancel .~ cancel restoreTask)

-- | Information we need to start the restoration process
data WalletInitInfo =
    -- | Create the wallet, without actually restoring
    --
   -- This is used only when the chain has no main blocks yet. We record
    -- the only the genesis UTxO for the wallet, and any addresses we found.
    WalletCreate
      (Map HD.HdAccountId (Utxo, [AddrWithId]))

    -- | Restore the wallet
    --
    -- We record the current and genesis UTxO, as well as some information
    -- about the most recent main block on the chain.
  | WalletRestore
      (Map HD.HdAccountId (Utxo, Utxo, [AddrWithId]))
      (HeaderHash, SlotId)

-- | Query the underlying node for the info we need to restore a wallet
--
-- We return the current and genesis UTxO for this wallet, as well some
-- information about the tip of the blockchain (provided the blockchain
-- isn't empty).
getWalletInitInfo :: NodeConstraints
                  => WalletKey
                  -> Lock (WithNodeState IO)
                  -> WithNodeState IO WalletInitInfo
getWalletInitInfo wKey@(wId, wdc) lock = do
    -- Find all of the current UTXO that this wallet owns.
    -- We lock the node state to be sure the tip header and the UTxO match
    (tipHeader, curUtxo :: Map HD.HdAccountId (Utxo, [AddrWithId])) <-
        fmap (second (fmap toPrefilteredUtxo . mergeUtxos)) $
          lock NotYetLocked $ \tip -> (tip, ) <$> filterUtxo isOurs

    -- Find genesis UTxO for this wallet
    let genUtxo :: Map HD.HdAccountId (Utxo, [AddrWithId])
        genUtxo = fmap toPrefilteredUtxo . snd $
                    prefilterUtxo' wKey (unGenesisUtxo genesisUtxo)

    -- Get the tip
    mTip <- mostRecentMainBlock tipHeader
    return $ case mTip of
      Nothing  -> WalletCreate genUtxo
      Just tip -> WalletRestore (mergeInfo curUtxo genUtxo) (tipInfo tip)
  where
    tipInfo :: MainBlock -> (HeaderHash, SlotId)
    tipInfo mb = (headerHash mb, mb ^. mainBlockSlot)

    mergeInfo :: (Monoid cur, Monoid gen)
              => Map HD.HdAccountId (cur, [AddrWithId])
              -> Map HD.HdAccountId (gen, [AddrWithId])
              -> Map HD.HdAccountId (cur, gen, [AddrWithId])
    mergeInfo = M.merge
        (M.mapMaybeMissing     $ \_ (c, as) -> Just (c, mempty, as))
        (M.mapMaybeMissing     $ \_ (g, as) -> Just (mempty, g, as))
        (M.zipWithMaybeMatched $ \_ (c, as) (g, as') -> Just (c, g, as ++ as'))

    mergeUtxos :: [(HD.HdAccountId, UtxoWithAddrId)]
               -> Map HD.HdAccountId UtxoWithAddrId
    mergeUtxos = M.fromListWith M.union

    isOurs :: (TxIn, TxOutAux) -> Maybe (HD.HdAccountId, UtxoWithAddrId)
    isOurs (inp, out@(TxOutAux (TxOut addr _))) = do
        wam <- decryptAddress wdc addr
        let addrId = toHdAddressId wId wam
        return (addrId ^. HD.hdAddressIdParent, M.singleton inp (out, addrId))

-- | Restore a wallet's transaction history.
--
-- TODO: Think about what we should do if a 'RestorationException' is thrown.
restoreWalletHistoryAsync :: Kernel.PassiveWallet
                          -> HD.HdRootId
                          -> (Blund -> IO (Map HD.HdAccountId PrefilteredBlock, [TxMeta]))
                          -> (HeaderHash, SlotId)
                          -> IO ()
restoreWalletHistoryAsync wallet rootId prefilter (tgtHash, tgtSlot) =
    -- 'getFirstGenesisBlockHash' is confusingly named: it returns the hash of
    -- the first block /after/ the genesis block.
    withNode getFirstGenesisBlockHash >>= restore NoTimingData
  where
    wId :: WalletId
    wId = WalletIdHdRnd rootId

    -- Process the restoration of the block with the given 'HeaderHash'.
    restore :: TimingData -> HeaderHash -> IO ()
    restore timing hh = do

        -- Updating the average rate every 5 blocks.
        (rate, timing') <- tickTiming 5 timing

        -- Update each account's historical checkpoints
        block <- getBlockOrThrow hh

        -- Skip EBBs
        whenRight block $ \mb -> do
            -- Filter the blocks by account
            blund <- (Right mb, ) <$> getUndoOrThrow hh
            (prefilteredBlocks, txMetas) <- prefilter blund

            -- Apply the block
            k    <- getSecurityParameter (wallet ^. walletNode)
            ctxt <- withNode $ mainBlockContext mb
            mErr <- update (wallet ^. wallets) $
                   ApplyHistoricalBlock k ctxt prefilteredBlocks
            case mErr of
                Left err -> throwM $ RestorationApplyHistoricalBlockFailed err
                Right () -> return ()

            -- Update our progress
            slotCount <- getSlotCount (wallet ^. walletNode)
            let flat             = flattenSlotId slotCount
                blockPerSec      = MeasuredIn . BlockCount . perSecond <$> rate
                throughputUpdate = maybe identity (set wriThroughput) blockPerSec
                slotId           = mb ^. mainBlockSlot
            updateRestorationInfo wallet wId ( (wriCurrentSlot .~ flat slotId)
                                             . (wriTargetSlot  .~ flat tgtSlot)
                                             . throughputUpdate )
            -- Store the TxMetas
            forM_ txMetas (putTxMeta (wallet ^. walletMeta))

        -- Decide how to proceed.
        if tgtHash == hh then
            finish
          else nextHistoricalHash hh >>= \case
            Nothing  -> throwM (RestorationFinishUnreachable tgtHash hh)
            Just hh' -> restore timing' hh'

    -- TODO (@mn): probably should use some kind of bracket to ensure this cleanup happens.
    finish :: IO ()
    finish = do
        k <- getSecurityParameter (wallet ^. walletNode)
        update (wallet ^. wallets) $ RestorationComplete k rootId
        modifyMVar_ (wallet ^. walletRestorationTask) (pure . M.delete wId)

    -- Step forward to the successor of the given block.
    nextHistoricalHash :: HeaderHash -> IO (Maybe HeaderHash)
    nextHistoricalHash hh = withNode $ resolveForwardLink hh

    -- Get a block
    getBlockOrThrow :: HeaderHash -> IO Block
    getBlockOrThrow hh = do
        mBlock <- withNode $ getBlock hh
        case mBlock of
           Nothing -> throwM $ RestorationBlockNotFound hh
           Just b  -> return b

    -- Get undo for a mainblock
    -- NOTE: We use this undo information only for input resolution.
    getUndoOrThrow :: HeaderHash -> IO Undo
    getUndoOrThrow hh = do
        mBlock <- withNode $ getUndo hh
        case mBlock of
           Nothing -> throwM $ RestorationUndoNotFound hh
           Just b  -> return b

    withNode :: forall a. (NodeConstraints => WithNodeState IO a) -> IO a
    withNode action = withNodeState (wallet ^. walletNode) (\_lock -> action)

-- Update the restoration information for a wallet. If somebody else is holding
-- the 'walletRestorationTask' MVar, we'll just skip this; the worst that can
-- happen is the restoration's progress information is not quite up-to-date.
--
-- The idea is that the wallet restoration should be thought of
-- as properly nested inside the WalletRestorationInfo's MVar. So to avoid
-- deadlock, we can't grab the WalletRestorationInfo MVar unconditionally here.
--
-- Under normal circumstances, there should not be much contention for the
-- WalletRestorationInfo MVar, and we expect this function to succeed.
updateRestorationInfo :: Kernel.PassiveWallet
                      -> WalletId
                      -> (WalletRestorationInfo -> WalletRestorationInfo)
                      -> IO ()
updateRestorationInfo wallet wId upd =
    tryTakeMVar wrt >>= \case
        Nothing   -> return ()
        Just info -> void $ tryPutMVar wrt (M.adjust upd wId info)
  where wrt = wallet ^. walletRestorationTask

{-------------------------------------------------------------------------------
  Timing information (for throughput calculations)
-------------------------------------------------------------------------------}

-- | Keep track of how many events have happened since a given start time.
data TimingData
  = NoTimingData
  | Timing Integer UTCTime

-- | A rate, represented as an event count over a time interval.
data Rate = Rate Integer NominalDiffTime

-- | Log an event; once k' events have been seen, return the event rate
-- and start the count over again.
tickTiming :: Integer -> TimingData -> IO (Maybe Rate, TimingData)
tickTiming _  NoTimingData     = (Nothing,) . Timing 0 <$> getCurrentTime
tickTiming k' (Timing k start)
  | k == k' = do
        now <- getCurrentTime
        let rate = Rate k (now `diffUTCTime` start)
        return (Just rate, Timing 0 now)
  | otherwise = return (Nothing, Timing (k + 1) start)

-- | Convert a rate to a number of events per second.
perSecond :: Rate -> Word64
perSecond (Rate n dt) = fromInteger $ round (toRational n / toRational dt)

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | Exception during restoration
data RestorationException =
    RestorationBlockNotFound HeaderHash
  | RestorationUndoNotFound HeaderHash
  | RestorationApplyHistoricalBlockFailed Spec.ApplyBlockFailed
  | RestorationFinishUnreachable HeaderHash HeaderHash

instance Buildable RestorationException where
    build (RestorationBlockNotFound hash) =
      bprint ("RestorationBlockNotFound " % build) hash
    build (RestorationUndoNotFound hash) =
      bprint ("RestorationUndoNotFound " % build) hash
    build (RestorationApplyHistoricalBlockFailed err) =
      bprint ("RestorationApplyHistoricalBlockFailed " % build) err
    build (RestorationFinishUnreachable target final) =
      bprint ("RestorationFinishUnreachable " % build % " " % build) target final

instance Show RestorationException where
    show = formatToString build

instance Exception RestorationException
