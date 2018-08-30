{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Pure Toss.

module Pos.Chain.Ssc.Toss.Pure
       ( PureToss (..)
       , PureTossWithEnv (..)
       , MultiRichmenStakes
       , MultiRichmenSet
       , runPureToss
       , runPureTossWithLogger
       , evalPureTossWithLogger
       , execPureTossWithLogger
       , supplyPureTossEnv
       ) where

import           Universum hiding (id)

import           Control.Lens (at, uses, (%=), (.=))
import           Control.Monad.Trans.Writer.Lazy (WriterT, runWriterT, tell)
import qualified Crypto.Random as Rand
import           Data.Sequence (singleton)

import           Pos.Chain.Lrc (RichmenSet, RichmenStakes)
import           Pos.Chain.Ssc.Base (deleteSignedCommitment,
                     insertSignedCommitment)
import           Pos.Chain.Ssc.Toss.Class (MonadToss (..), MonadTossEnv (..),
                     MonadTossRead (..))
import           Pos.Chain.Ssc.Types (SscGlobalState, sgsCommitments,
                     sgsOpenings, sgsShares, sgsVssCertificates)
import qualified Pos.Chain.Ssc.VssCertData as VCD
import           Pos.Core (EpochIndex, HasGenesisData, crucialSlot,
                     genesisVssCerts)
import           Pos.Core.Update (BlockVersionData)
import           Pos.Util.Wlog (CanLog (..), HasLoggerName (..), LogEvent (..),
                     WithLogger, dispatchEvents)

type MultiRichmenStakes = HashMap EpochIndex RichmenStakes
type MultiRichmenSet   = HashMap EpochIndex RichmenSet

-- 'MonadPseudoRandom' is needed because some cryptographic algorithms
-- require randomness even though they are deterministic. Note that running
-- them with the same seed every time is insecure and must not be done.
newtype PureToss a = PureToss
    { getPureToss :: StateT SscGlobalState (
                     WriterT (Seq LogEvent) (
                     Rand.MonadPseudoRandom Rand.ChaChaDRG)) a
    } deriving (Functor, Applicative, Monad,
                CanLog, HasLoggerName, Rand.MonadRandom)

instance Monad m => CanLog (WriterT (Seq LogEvent) m) where
    dispatchMessage name sev msg = tell (singleton (LogEvent name sev msg))

newtype PureTossWithEnv a = PureTossWithEnv
    { getPureTossWithEnv ::
          ReaderT (MultiRichmenStakes, BlockVersionData) PureToss a
    } deriving (Functor, Applicative, Monad, Rand.MonadRandom,
                CanLog, HasLoggerName)

deriving instance HasGenesisData => MonadTossRead PureTossWithEnv
deriving instance HasGenesisData => MonadToss PureTossWithEnv

instance HasGenesisData => MonadTossRead PureToss where
    getCommitments = PureToss $ use sgsCommitments
    getOpenings = PureToss $ use sgsOpenings
    getShares = PureToss $ use sgsShares
    getVssCertificates = PureToss $ uses sgsVssCertificates VCD.certs
    getStableCertificates k epoch
        | epoch == 0 = pure $ genesisVssCerts
        | otherwise = PureToss $
            uses sgsVssCertificates $
                VCD.certs . VCD.setLastKnownSlot (crucialSlot k epoch)

instance MonadTossEnv PureTossWithEnv where
    getRichmen epoch = PureTossWithEnv $ view (_1 . at epoch)
    getAdoptedBVData = PureTossWithEnv $ view _2

instance HasGenesisData => MonadToss PureToss where
    putCommitment signedComm =
        PureToss $ sgsCommitments %= insertSignedCommitment signedComm
    putOpening id op = PureToss $ sgsOpenings . at id .= Just op
    putShares id sh = PureToss $ sgsShares . at id .= Just sh
    putCertificate cert =
        PureToss $ sgsVssCertificates %= VCD.insert cert
    delCommitment id =
        PureToss $ sgsCommitments %= deleteSignedCommitment id
    delOpening id = PureToss $ sgsOpenings . at id .= Nothing
    delShares id = PureToss $ sgsShares . at id .= Nothing
    resetCO = PureToss $ do
        sgsCommitments .= mempty
        sgsOpenings .= mempty
    resetShares = PureToss $ sgsShares .= mempty
    setEpochOrSlot eos = PureToss $ sgsVssCertificates %= VCD.setLastKnownEoS eos

runPureToss
    :: Rand.MonadRandom m
    => SscGlobalState
    -> PureToss a
    -> m (a, SscGlobalState, Seq LogEvent)
runPureToss gs (PureToss act) = do
    seed <- Rand.drgNew
    let ((res, newGS), events) =
            fst . Rand.withDRG seed $    -- run MonadRandom
            runWriterT $                 -- run Writer of Seq LogEvent
            runStateT act gs             -- run State
    pure (res, newGS, events)

runPureTossWithLogger
    :: (WithLogger m, Rand.MonadRandom m)
    => SscGlobalState
    -> PureToss a
    -> m (a, SscGlobalState)
runPureTossWithLogger gs act = do
    (res, newGS, events) <- runPureToss gs act
    (res, newGS) <$ (dispatchEvents $ toList events)

evalPureTossWithLogger
    :: (WithLogger m, Rand.MonadRandom m)
    => SscGlobalState
    -> PureToss a
    -> m a
evalPureTossWithLogger g = fmap fst . runPureTossWithLogger g

execPureTossWithLogger
    :: (WithLogger m, Rand.MonadRandom m)
    => SscGlobalState
    -> PureToss a
    -> m SscGlobalState
execPureTossWithLogger g = fmap snd . runPureTossWithLogger g

supplyPureTossEnv
    :: (MultiRichmenStakes, BlockVersionData)
    -> PureTossWithEnv a
    -> PureToss a
supplyPureTossEnv env = flip runReaderT env . getPureTossWithEnv
