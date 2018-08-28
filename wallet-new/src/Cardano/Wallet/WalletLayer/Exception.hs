{-# LANGUAGE ExistentialQuantification #-}

module Cardano.Wallet.WalletLayer.Exception
    ( WalletException(..)
    , walletExceptionToException
    , walletExceptionFromException
    ) where

import           Universum

import           Cardano.Wallet.API.Response.JSend (HasDiagnostic)
import           Cardano.Wallet.API.V1.Errors (ToHttpErrorStatus)

import           Data.Typeable (cast)

import qualified Prelude


data WalletException = forall e.
    ( Exception e
    , HasDiagnostic e
    , ToHttpErrorStatus e
    ) => WalletException { getWalletException :: e }

instance Prelude.Show WalletException where
    show (WalletException e) = Prelude.show e

instance Exception WalletException


walletExceptionToException :: forall e.
    ( Exception e
    , HasDiagnostic e
    , ToHttpErrorStatus e
    ) => e -> SomeException
walletExceptionToException = toException . WalletException

walletExceptionFromException :: Exception e => SomeException -> Maybe e
walletExceptionFromException x = do
    WalletException a <- fromException x
    cast a
