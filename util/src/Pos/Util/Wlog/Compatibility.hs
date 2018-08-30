module Pos.Util.Wlog.Compatibility
        ( -- * LoggerName
          LoggerNameBox (..)
        , HasLoggerName (..)
        , usingLoggerName
        ) where

import           Universum

import           Control.Monad.Base (MonadBase)
import           Control.Monad.Except (MonadError)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.Morph (MFunctor (..))
import qualified Control.Monad.State as StateLazy (StateT)
-- import           Control.Monad.Trans.Control (MonadBaseControl (..))
import           Control.Monad.Writer (WriterT (..))


import           Pos.Util.Log (LoggerName)

-- HasLoggerName

class HasLoggerName m where
    -- | Extract logger name from context
    askLoggerName :: m LoggerName

    -- | Change logger name in context
    modifyLoggerName :: (LoggerName -> LoggerName) -> m a -> m a

    default askLoggerName :: (MonadTrans t, t n ~ m, Monad n, HasLoggerName n) => m LoggerName
    askLoggerName = lift askLoggerName

    default modifyLoggerName :: (MFunctor t, t n ~ m, Monad n, HasLoggerName n)
                             => (LoggerName -> LoggerName)
                             -> m a
                             -> m a
    modifyLoggerName f = hoist (modifyLoggerName f)

instance (Monad m, HasLoggerName m) => HasLoggerName (ReaderT a m) where
instance (Monad m, HasLoggerName m) => HasLoggerName (StateT a m) where
instance (Monad m, HasLoggerName m) => HasLoggerName (StateLazy.StateT a m) where
instance (Monoid w, Monad m, HasLoggerName m) => HasLoggerName (WriterT w m) where
instance (Monad m, HasLoggerName m) => HasLoggerName (ExceptT e m) where

-- LoggerNameBox

-- | Default implementation of `WithNamedLogger`.
newtype LoggerNameBox m a = LoggerNameBox
    { loggerNameBoxEntry :: ReaderT LoggerName m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadBase b,
                MonadThrow, MonadCatch, MonadMask, MonadError e, MonadState s,
                MonadFix)

instance MonadReader r m => MonadReader r (LoggerNameBox m) where
    ask = lift ask
    reader = lift . reader
    local f (LoggerNameBox m) = askLoggerName >>= lift . local f . runReaderT m

-- | Runs a `LoggerNameBox` with specified initial `LoggerName`.
usingLoggerName :: LoggerName -> LoggerNameBox m a -> m a
usingLoggerName name = flip runReaderT name . loggerNameBoxEntry

instance Monad m => HasLoggerName (LoggerNameBox m) where
    askLoggerName = LoggerNameBox ask
    modifyLoggerName how = LoggerNameBox . local how . loggerNameBoxEntry
