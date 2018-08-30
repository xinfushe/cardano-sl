{-# LANGUAGE RankNTypes #-}

-- | Utilities for manipulating in-memory SSC state.

-- TODO: the names for functions are pretty bad
module Pos.Chain.Ssc.Mem
       (
       -- * 'MonadSscMem'
         SscMemTag
       , MonadSscMem
       , askSscMem
       , syncingStateWith

       -- * Local state
       , SscLocalQuery
       , SscLocalUpdate
       , sscRunLocalQuery
       , sscRunLocalSTM

       -- * Global state
       , SscGlobalQuery
       , SscGlobalUpdate
       , sscRunGlobalQuery
       , sscRunGlobalUpdate
       ) where

import           Universum

import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Writer.Lazy (WriterT, runWriterT)
import qualified Crypto.Random as Rand

import           Pos.Chain.Ssc.Types (SscGlobalState, SscLocalData, SscState,
                     sscGlobal, sscLocal)
import           Pos.Util.Util (HasLens (..))
import           Pos.Util.Wlog (LogEvent (..), WithLogger, dispatchEvents)

----------------------------------------------------------------------------
-- MonadSscMem
----------------------------------------------------------------------------

data SscMemTag

type MonadSscMem ctx m = (MonadReader ctx m, HasLens SscMemTag ctx SscState)

askSscMem :: MonadSscMem ctx m => m SscState
askSscMem = view (lensOf @SscMemTag)

-- | Applies state changes to given var.
syncingStateWith
    :: TVar s
    -> StateT s STM a
    -> STM a
syncingStateWith var action = do
    oldV <- readTVar var
    (res, newV) <- runStateT action oldV
    writeTVar var newV
    return res

----------------------------------------------------------------------------
-- Local
----------------------------------------------------------------------------

type SscLocalQuery a =
    forall m . (MonadReader SscLocalData m, WithLogger m) => m a

type SscLocalUpdate a =
    forall m . (MonadState SscLocalData m, WithLogger m, Rand.MonadRandom m)
        => WriterT (Seq LogEvent) m a

-- | Run something that reads 'SscLocalData' in 'MonadSscMem'.
-- 'MonadIO' is also needed to use stm.
sscRunLocalQuery
    :: forall ctx m a.
       (MonadSscMem ctx m, MonadIO m)
    => ReaderT SscLocalData m a -> m a
sscRunLocalQuery action = do
    localVar <- sscLocal <$> askSscMem
    ld <- readTVarIO localVar
    runReaderT action ld

-- | Run STM transaction which modifies 'SscLocalData' and also can log.
sscRunLocalSTM
    :: forall ctx m a.
       (MonadSscMem ctx m, MonadIO m, WithLogger m)
    => WriterT (Seq LogEvent) (StateT SscLocalData STM) a -> m a
sscRunLocalSTM action = do
    localVar <- sscLocal <$> askSscMem
    (a, logEvents) <- atomically $ syncingStateWith localVar $ runWriterT action
    dispatchEvents $ toList logEvents
    pure a

----------------------------------------------------------------------------
-- Global
----------------------------------------------------------------------------

type SscGlobalQuery a =
    forall m . (MonadReader SscGlobalState m) => m a

type SscGlobalUpdate a =
    forall m . (MonadState SscGlobalState m, WithLogger m, Rand.MonadRandom m) => m a

-- | Run something that reads 'SscGlobalState' in 'MonadSscMem'.
-- 'MonadIO' is also needed to use stm.
sscRunGlobalQuery
    :: (MonadSscMem ctx m, MonadIO m)
    => ReaderT SscGlobalState m a -> m a
sscRunGlobalQuery action = do
    globalVar <- sscGlobal <$> askSscMem
    gs <- readTVarIO globalVar
    runReaderT action gs

sscRunGlobalUpdate
    :: (MonadSscMem ctx m, Rand.MonadRandom m, WithLogger m, MonadIO m)
    => WriterT (Seq LogEvent) (StateT SscGlobalState (Rand.MonadPseudoRandom Rand.ChaChaDRG)) a
    -> m a
sscRunGlobalUpdate action = do
    globalVar <- sscGlobal <$> askSscMem
    seed <- Rand.drgNew
    (a, logEvents) <- atomically $
        syncingStateWith globalVar $
        runWriterT $
        executeMonadBaseRandom seed action
    dispatchEvents $ toList logEvents
    pure a
  where
    -- (... MonadPseudoRandom) a -> (... n) a
    executeMonadBaseRandom seed =
        hoist $ hoist (pure . fst . Rand.withDRG seed)
