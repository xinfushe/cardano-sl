{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module Cardano.Wallet.WalletLayer
    ( PassiveWalletLayer (..)
    , ActiveWalletLayer (..)
    -- * Errors
    , CreateWalletError(..)
    , GetWalletError(..)
    , UpdateWalletError(..)
    , UpdateWalletPasswordError(..)
    , DeleteWalletError(..)
    , GetUtxosError(..)
    , NewPaymentError(..)
    , EstimateFeesError(..)
    , RedeemAdaError(..)
    , CreateAddressError(..)
    , ValidateAddressError(..)
    , CreateAccountError(..)
    , GetAccountError(..)
    , GetAccountsError(..)
    , GetTxError(..)
    , DeleteAccountError(..)
    , UpdateAccountError(..)
    ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Formatting (bprint, build, formatToString, (%))
import qualified Formatting.Buildable
import           Generics.SOP.TH (deriveGeneric)
import qualified Prelude
import           Servant (err400, err404)

import           Pos.Chain.Block (Blund)
import           Pos.Chain.Txp (Utxo)
import           Pos.Core (Coin, Timestamp)
import           Pos.Core.Chrono (NE, NewestFirst (..), OldestFirst (..))
import           Pos.Core.Txp (Tx, TxId)
import           Pos.Core.Update (SoftwareVersion)
import           Pos.Crypto (PassPhrase)

import           Cardano.Wallet.API.Request (RequestParams (..))
import           Cardano.Wallet.API.Request.Filter (FilterOperations (..))
import           Cardano.Wallet.API.Request.Sort (SortOperations (..))
import           Cardano.Wallet.API.Response (SliceOf (..), WalletResponse)
import           Cardano.Wallet.API.Response.JSend (HasDiagnostic (..))
import           Cardano.Wallet.API.V1.Errors (ToHttpErrorStatus,
                     ToServantError (..))
import           Cardano.Wallet.API.V1.Generic (jsendErrorGenericParseJSON,
                     jsendErrorGenericToJSON)
import           Cardano.Wallet.API.V1.Types (Account, AccountBalance,
                     AccountIndex, AccountUpdate, Address, ForceNtpCheck,
                     NewAccount, NewAddress, NewWallet, NodeInfo, NodeSettings,
                     PasswordUpdate, Payment, Redemption, Transaction, V1 (..),
                     Wallet, WalletAddress, WalletId, WalletUpdate)
import qualified Cardano.Wallet.Kernel.Accounts as Kernel
import qualified Cardano.Wallet.Kernel.Addresses as Kernel
import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
                     (ExpenseRegulation, InputGrouping)
import           Cardano.Wallet.Kernel.DB.HdWallet ()
import qualified Cardano.Wallet.Kernel.DB.HdWallet as Kernel
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.DB.Util.IxSet (IxSet)
import qualified Cardano.Wallet.Kernel.Transactions as Kernel
import qualified Cardano.Wallet.Kernel.Wallets as Kernel
import           Cardano.Wallet.WalletLayer.Exception
                     (walletExceptionFromException, walletExceptionToException)
import           Cardano.Wallet.WalletLayer.ExecutionTimeLimit
                     (TimeExecutionLimit)
import           Cardano.Wallet.WalletLayer.Kernel.Conv (InvalidRedemptionCode)

------------------------------------------------------------
-- Errors when manipulating wallets
------------------------------------------------------------

data CreateWalletError =
      CreateWalletError Kernel.CreateWalletError
    | CreateWalletFirstAccountCreationFailed Kernel.CreateAccountError
    deriving (Generic, Eq)

deriveGeneric ''CreateWalletError

instance HasDiagnostic CreateWalletError where
    getDiagnosticKey = \case
        CreateWalletError _ -> "TODO"
        CreateWalletFirstAccountCreationFailed _ -> "TODO"

instance ToServantError CreateWalletError where
    declareServantError = \case
        CreateWalletError _ -> err400 -- FIXME: Is this correct?
        CreateWalletFirstAccountCreationFailed _ -> err400 -- FIXME: Is this correct?

instance ToHttpErrorStatus CreateWalletError

instance ToJSON CreateWalletError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON CreateWalletError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable CreateWalletError where
    build (CreateWalletError kernelError) =
        bprint ("CreateWalletError " % build) kernelError
    build (CreateWalletFirstAccountCreationFailed kernelError) =
        bprint ("CreateWalletFirstAccountCreationFailed " % build) kernelError

-- | Unsound show instance needed for the 'Exception' instance.
instance Show CreateWalletError where
    show = formatToString build

instance Exception CreateWalletError where
    toException   = walletExceptionToException
    fromException = walletExceptionFromException

data GetWalletError =
      GetWalletError (V1 Kernel.UnknownHdRoot)
    | GetWalletErrorNotFound WalletId
    -- ^ Error thrown by the legacy wallet layer, isomorphic to the one above,
    -- which is new-data-layer specific.
    | GetWalletWalletIdDecodingFailed Text
    deriving (Generic, Eq)

deriveGeneric ''GetWalletError

instance HasDiagnostic GetWalletError where
    getDiagnosticKey = \case
        GetWalletError _ -> "TODO"
        GetWalletErrorNotFound _ -> "TODO"
        GetWalletWalletIdDecodingFailed _ -> "TODO"

instance ToServantError GetWalletError where
    declareServantError = \case
        GetWalletError _ -> err400 -- FIXME: Is this correct?
        GetWalletErrorNotFound _ -> err400 -- FIXME: Is this correct?
        GetWalletWalletIdDecodingFailed _ -> err400 -- FIXME: Is this correct?

instance ToHttpErrorStatus GetWalletError

instance ToJSON GetWalletError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON GetWalletError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable GetWalletError where
    build (GetWalletError (V1 kernelError)) =
        bprint ("GetWalletError " % build) kernelError
    build (GetWalletErrorNotFound walletId) =
        bprint ("GetWalletErrorNotFound " % build) walletId
    build (GetWalletWalletIdDecodingFailed txt) =
        bprint ("GetWalletWalletIdDecodingFailed " % build) txt

-- | Unsound show instance needed for the 'Exception' instance.
instance Show GetWalletError where
    show = formatToString build

instance Exception GetWalletError where
    toException   = walletExceptionToException
    fromException = walletExceptionFromException

data UpdateWalletError =
      UpdateWalletError (V1 Kernel.UnknownHdRoot)
    | UpdateWalletErrorNotFound WalletId
    -- ^ Error thrown by the legacy wallet layer, isomorphic to the one above,
    -- which is new-data-layer specific.
    | UpdateWalletWalletIdDecodingFailed Text
    deriving (Generic, Eq)

deriveGeneric ''UpdateWalletError

instance HasDiagnostic UpdateWalletError where
    getDiagnosticKey = \case
        UpdateWalletError _ -> "TODO"
        UpdateWalletErrorNotFound _ -> "TODO"
        UpdateWalletWalletIdDecodingFailed _ -> "TODO"

instance ToServantError UpdateWalletError where
    declareServantError = \case
        UpdateWalletError _ -> err400 -- FIXME: Is this correct?
        UpdateWalletErrorNotFound _ -> err400 -- FIXME: Is this correct?
        UpdateWalletWalletIdDecodingFailed _ -> err400 -- FIXME: Is this correct?

instance ToHttpErrorStatus UpdateWalletError

instance ToJSON UpdateWalletError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON UpdateWalletError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable UpdateWalletError where
    build (UpdateWalletError (V1 kernelError)) =
        bprint ("UpdateWalletError " % build) kernelError
    build (UpdateWalletErrorNotFound walletId) =
        bprint ("UpdateWalletErrorNotFound " % build) walletId
    build (UpdateWalletWalletIdDecodingFailed txt) =
        bprint ("UpdateWalletWalletIdDecodingFailed " % build) txt

-- | Unsound show instance needed for the 'Exception' instance.
instance Show UpdateWalletError where
    show = formatToString build

instance Exception UpdateWalletError where
    toException   = walletExceptionToException
    fromException = walletExceptionFromException


data UpdateWalletPasswordError =
      UpdateWalletPasswordWalletIdDecodingFailed Text
    | UpdateWalletPasswordError Kernel.UpdateWalletPasswordError
    deriving (Generic, Eq)

deriveGeneric ''UpdateWalletPasswordError

instance HasDiagnostic UpdateWalletPasswordError where
    getDiagnosticKey = \case
        UpdateWalletPasswordWalletIdDecodingFailed _ -> "TODO"
        UpdateWalletPasswordError _ -> "TODO"

instance ToServantError UpdateWalletPasswordError where
    declareServantError = \case
        UpdateWalletPasswordWalletIdDecodingFailed _ -> err400 -- FIXME: Is this correct?
        UpdateWalletPasswordError _ -> err400 -- FIXME: Is this correct?

instance ToHttpErrorStatus UpdateWalletPasswordError

instance ToJSON UpdateWalletPasswordError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON UpdateWalletPasswordError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable UpdateWalletPasswordError where
    build (UpdateWalletPasswordWalletIdDecodingFailed txt) =
        bprint ("UpdateWalletPasswordWalletIdDecodingFailed " % build) txt
    build (UpdateWalletPasswordError kernelError) =
        bprint ("UpdateWalletPasswordError " % build) kernelError

-- | Unsound show instance needed for the 'Exception' instance.
instance Show UpdateWalletPasswordError where
    show = formatToString build

instance Exception UpdateWalletPasswordError where
    toException   = walletExceptionToException
    fromException = walletExceptionFromException

data DeleteWalletError =
      DeleteWalletWalletIdDecodingFailed Text
    | DeleteWalletError (V1 Kernel.UnknownHdRoot)
    deriving (Generic, Eq)

deriveGeneric ''DeleteWalletError

instance HasDiagnostic DeleteWalletError where
    getDiagnosticKey = \case
        DeleteWalletWalletIdDecodingFailed _ -> "TODO"
        DeleteWalletError _ -> "TODO"

instance ToServantError DeleteWalletError where
    declareServantError = \case
        DeleteWalletWalletIdDecodingFailed _ -> err400 -- FIXME: Is this correct?
        DeleteWalletError _ -> err400 -- FIXME: Is this correct?

instance ToHttpErrorStatus DeleteWalletError

instance ToJSON DeleteWalletError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON DeleteWalletError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable DeleteWalletError where
    build (DeleteWalletWalletIdDecodingFailed txt) =
        bprint ("DeleteWalletWalletIdDecodingFailed " % build) txt
    build (DeleteWalletError kernelError) =
        bprint ("DeleteWalletError " % build) kernelError

-- | Unsound show instance needed for the 'Exception' instance.
instance Show DeleteWalletError where
    show = formatToString build

instance Exception DeleteWalletError where
    toException   = walletExceptionToException
    fromException = walletExceptionFromException


data GetUtxosError =
      GetUtxosWalletIdDecodingFailed Text
    | GetUtxosGetAccountsError Kernel.UnknownHdRoot
    | GetUtxosCurrentAvailableUtxoError Kernel.UnknownHdAccount
    deriving (Generic, Eq)

deriveGeneric ''GetUtxosError

instance HasDiagnostic GetUtxosError where
    getDiagnosticKey = \case
        GetUtxosWalletIdDecodingFailed _ -> "TODO"
        GetUtxosGetAccountsError _ -> "TODO"
        GetUtxosCurrentAvailableUtxoError _ -> "TODO"

instance ToServantError GetUtxosError where
    declareServantError = \case
        GetUtxosWalletIdDecodingFailed _ -> err400 -- FIXME: Is this correct?
        GetUtxosGetAccountsError _ -> err400 -- FIXME: Is this correct?
        GetUtxosCurrentAvailableUtxoError _ -> err400 -- FIXME: Is this correct?

instance ToHttpErrorStatus GetUtxosError

instance ToJSON GetUtxosError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON GetUtxosError where
    parseJSON = jsendErrorGenericParseJSON

-- | Unsound show instance needed for the 'Exception' instance.
instance Show GetUtxosError where
    show = formatToString build

instance Exception GetUtxosError

instance Buildable GetUtxosError where
    build (GetUtxosWalletIdDecodingFailed txt) =
        bprint ("GetUtxosWalletIdDecodingFailed " % build) txt
    build (GetUtxosGetAccountsError kernelError) =
        bprint ("GetUtxosGetAccountsError " % build) kernelError
    build (GetUtxosCurrentAvailableUtxoError kernelError) =
        bprint ("GetUtxosCurrentAvailableUtxoError " % build) kernelError

------------------------------------------------------------
-- Errors when dealing with addresses
------------------------------------------------------------

data CreateAddressError =
      CreateAddressError Kernel.CreateAddressError
    | CreateAddressAddressDecodingFailed Text
    -- ^ Decoding the input 'Text' as an 'Address' failed.
    deriving (Generic, Eq)

deriveGeneric ''CreateAddressError

instance HasDiagnostic CreateAddressError where
    getDiagnosticKey = \case
        CreateAddressError _ -> "TODO"
        CreateAddressAddressDecodingFailed _ -> "TODO"

instance ToServantError CreateAddressError where
    declareServantError = \case
        CreateAddressError _ -> err400 -- FIXME: Is this correct?
        CreateAddressAddressDecodingFailed _ -> err400 -- FIXME: Is this correct?

instance ToHttpErrorStatus CreateAddressError

instance ToJSON CreateAddressError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON CreateAddressError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable CreateAddressError where
    build (CreateAddressError kernelError) =
        bprint ("CreateAddressError " % build) kernelError
    build (CreateAddressAddressDecodingFailed txt) =
        bprint ("CreateAddressAddressDecodingFailed " % build) txt

-- | Unsound show instance needed for the 'Exception' instance.
instance Show CreateAddressError where
    show = formatToString build

instance Exception CreateAddressError where
    toException   = walletExceptionToException
    fromException = walletExceptionFromException


data ValidateAddressError =
      ValidateAddressDecodingFailed Text
    -- ^ When trying to decode this raw 'Text' into a proper Cardano
    -- 'Address' the decoding failed. Unfortunately we are not able to
    -- provide a more accurate error description as 'decodeTextAddress' doesn't
    -- offer such.
    deriving (Generic, Eq)

deriveGeneric ''ValidateAddressError

instance HasDiagnostic ValidateAddressError where
    getDiagnosticKey = \case
        ValidateAddressDecodingFailed _ -> "TODO"

instance ToServantError ValidateAddressError where
    declareServantError = \case _ -> err400 -- FIXME: Is this correct?

instance ToHttpErrorStatus ValidateAddressError

instance ToJSON ValidateAddressError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON ValidateAddressError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable ValidateAddressError where
    build (ValidateAddressDecodingFailed rawText) =
        bprint ("ValidateAddressDecodingFailed " % build) rawText

-- | Unsound show instance needed for the 'Exception' instance.
instance Show ValidateAddressError where
    show = formatToString build

instance Exception ValidateAddressError where
    toException   = walletExceptionToException
    fromException = walletExceptionFromException

------------------------------------------------------------
-- Errors when dealing with Accounts
------------------------------------------------------------

data CreateAccountError =
      CreateAccountError Kernel.CreateAccountError
    | CreateAccountWalletIdDecodingFailed Text
    -- ^ Decoding the parent's 'WalletId' from a raw 'Text' failed.
    | CreateAccountFirstAddressGenerationFailed Kernel.CreateAddressError
    -- ^ When trying to create the first 'Address' to go in tandem with this
    -- 'Account', the generation failed.
    deriving (Generic, Eq)

deriveGeneric ''CreateAccountError

instance HasDiagnostic CreateAccountError where
    getDiagnosticKey = \case
        CreateAccountError _ -> "TODO"
        CreateAccountWalletIdDecodingFailed _ -> "TODO"
        CreateAccountFirstAddressGenerationFailed _ -> "TODO"

instance ToServantError CreateAccountError where
    declareServantError = \case
        CreateAccountError _ -> err400 -- FIXME: Is this correct?
        CreateAccountWalletIdDecodingFailed _ -> err400 -- FIXME: Is this correct?
        CreateAccountFirstAddressGenerationFailed _ -> err400 -- FIXME: Is this correct?

instance ToHttpErrorStatus CreateAccountError

instance ToJSON CreateAccountError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON CreateAccountError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable CreateAccountError where
    build (CreateAccountError kernelError) =
        bprint ("CreateAccountError " % build) kernelError
    build (CreateAccountWalletIdDecodingFailed txt) =
        bprint ("CreateAccountWalletIdDecodingFailed " % build) txt
    build (CreateAccountFirstAddressGenerationFailed kernelError) =
        bprint ("CreateAccountFirstAddressGenerationFailed " % build) kernelError

-- | Unsound show instance needed for the 'Exception' instance.
instance Show CreateAccountError where
    show = formatToString build

instance Exception CreateAccountError where
    toException   = walletExceptionToException
    fromException = walletExceptionFromException


data GetAccountError =
      GetAccountError (V1 Kernel.UnknownHdAccount)
    | GetAccountWalletIdDecodingFailed Text
    deriving (Generic, Eq)

deriveGeneric ''GetAccountError

instance HasDiagnostic GetAccountError where
    getDiagnosticKey = \case
        GetAccountError _ -> "TODO"
        GetAccountWalletIdDecodingFailed _ -> "TODO"

instance ToServantError GetAccountError where
    declareServantError = \case
        GetAccountError _ -> err400 -- FIXME: Is this correct?
        GetAccountWalletIdDecodingFailed _ -> err400 -- FIXME: Is this correct?

instance ToHttpErrorStatus GetAccountError

instance ToJSON GetAccountError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON GetAccountError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable GetAccountError where
    build (GetAccountError kernelError) =
        bprint ("GetAccountError " % build) kernelError
    build (GetAccountWalletIdDecodingFailed txt) =
        bprint ("GetAccountWalletIdDecodingFailed " % build) txt

-- | Unsound show instance needed for the 'Exception' instance.
instance Show GetAccountError where
    show = formatToString build

instance Exception GetAccountError where
    toException   = walletExceptionToException
    fromException = walletExceptionFromException


data DeleteAccountError =
      DeleteAccountError (V1 Kernel.UnknownHdAccount)
    | DeleteAccountWalletIdDecodingFailed Text
    deriving (Generic, Eq)

deriveGeneric ''DeleteAccountError

instance Buildable DeleteAccountError where
    build (DeleteAccountError kernelError) =
        bprint ("DeleteAccountError " % build) kernelError
    build (DeleteAccountWalletIdDecodingFailed txt) =
        bprint ("DeleteAccountWalletIdDecodingFailed " % build) txt

instance HasDiagnostic DeleteAccountError where
    getDiagnosticKey = \case
        DeleteAccountError _ -> "unknownAccount"
        DeleteAccountWalletIdDecodingFailed _ -> "message"

instance ToServantError DeleteAccountError where
    declareServantError = \case
        DeleteAccountError _ -> err404
        DeleteAccountWalletIdDecodingFailed _ -> err400

instance ToHttpErrorStatus DeleteAccountError

instance ToJSON DeleteAccountError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON DeleteAccountError where
    parseJSON = jsendErrorGenericParseJSON

-- | Unsound show instance needed for the 'Exception' instance.
instance Show DeleteAccountError where
    show = formatToString build

instance Exception DeleteAccountError where
    toException   = walletExceptionToException
    fromException = walletExceptionFromException


data GetAccountsError =
      GetAccountsError Kernel.UnknownHdRoot
    | GetAccountsWalletIdDecodingFailed Text
    deriving (Generic, Eq)

deriveGeneric ''GetAccountsError

instance HasDiagnostic GetAccountsError where
    getDiagnosticKey = \case
        GetAccountsError _                  -> "TODO"
        GetAccountsWalletIdDecodingFailed _ -> "TODO"

instance ToServantError GetAccountsError where
    declareServantError = \case
        GetAccountsError _ -> err404
        GetAccountsWalletIdDecodingFailed _ -> err400

instance ToHttpErrorStatus GetAccountsError

instance ToJSON GetAccountsError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON GetAccountsError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable GetAccountsError where
    build (GetAccountsError kernelError) =
        bprint ("GetAccountsError " % build) kernelError
    build (GetAccountsWalletIdDecodingFailed txt) =
        bprint ("GetAccountsWalletIdDecodingFailed " % build) txt

-- | Unsound show instance needed for the 'Exception' instance.
instance Show GetAccountsError where
    show = formatToString build

instance Exception GetAccountsError where
    toException   = walletExceptionToException
    fromException = walletExceptionFromException


data UpdateAccountError =
      UpdateAccountError (V1 Kernel.UnknownHdAccount)
    | UpdateAccountWalletIdDecodingFailed Text
    deriving (Generic, Eq)

deriveGeneric ''UpdateAccountError

instance HasDiagnostic UpdateAccountError where
    getDiagnosticKey = \case
        UpdateAccountError _ -> "unknownAccount"
        UpdateAccountWalletIdDecodingFailed _ -> "message"

instance ToServantError UpdateAccountError where
    declareServantError = \case
        UpdateAccountError _ -> err404
        UpdateAccountWalletIdDecodingFailed _ -> err400

instance ToHttpErrorStatus UpdateAccountError

instance ToJSON UpdateAccountError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON UpdateAccountError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable UpdateAccountError where
    build (UpdateAccountError kernelError) =
        bprint ("UpdateAccountError " % build) kernelError
    build (UpdateAccountWalletIdDecodingFailed txt) =
        bprint ("UpdateAccountWalletIdDecodingFailed " % build) txt

-- | Unsound show instance needed for the 'Exception' instance.
instance Show UpdateAccountError where
    show = formatToString build

instance Exception UpdateAccountError where
    toException   = walletExceptionToException
    fromException = walletExceptionFromException

------------------------------------------------------------
-- Errors when getting Transactions
------------------------------------------------------------

data GetTxError =
      GetTxMissingWalletIdError
    | GetTxAddressDecodingFailed Text
    | GetTxInvalidSortingOperaration String
    | GetTxUnknownHdAccount Kernel.UnknownHdAccount
    deriving (Generic, Eq)

deriveGeneric ''GetTxError

instance HasDiagnostic GetTxError where
    getDiagnosticKey = \case
        GetTxMissingWalletIdError -> "TODO"
        GetTxAddressDecodingFailed _ -> "TODO"
        GetTxInvalidSortingOperaration _ -> "TODO"
        GetTxUnknownHdAccount _ -> "TODO"

instance ToServantError GetTxError where
    declareServantError = \case
        GetTxMissingWalletIdError -> err400 -- FIXME: Is this correct?
        GetTxAddressDecodingFailed _ -> err400 -- FIXME: Is this correct?
        GetTxInvalidSortingOperaration _ -> err400 -- FIXME: Is this correct?
        GetTxUnknownHdAccount _ -> err400 -- FIXME: Is this correct?

instance ToHttpErrorStatus GetTxError

instance ToJSON GetTxError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON GetTxError where
    parseJSON = jsendErrorGenericParseJSON

instance Buildable GetTxError where
    build GetTxMissingWalletIdError =
        bprint "GetTxMissingWalletIdError "
    build (GetTxAddressDecodingFailed txt) =
        bprint ("GetTxAddressDecodingFailed " % build) txt
    build (GetTxInvalidSortingOperaration txt) =
        bprint ("GetTxInvalidSortingOperaration " % build) txt
    build (GetTxUnknownHdAccount err) =
        bprint ("GetTxUnknownHdAccount " % build) err

-- | Unsound show instance needed for the 'Exception' instance.
instance Show GetTxError where
    show = formatToString build

instance Exception GetTxError where
    toException   = walletExceptionToException
    fromException = walletExceptionFromException

------------------------------------------------------------
-- Active wallet errors
------------------------------------------------------------

data NewPaymentError =
      NewPaymentError Kernel.PaymentError
    | NewPaymentTimeLimitReached TimeExecutionLimit
    | NewPaymentWalletIdDecodingFailed Text
    | NewPaymentUnknownAccountId Kernel.UnknownHdAccount
    deriving (Generic, Eq)

-- | Unsound show instance needed for the 'Exception' instance.
instance Show NewPaymentError where
    show = formatToString build

deriveGeneric ''NewPaymentError

instance HasDiagnostic NewPaymentError where
    getDiagnosticKey = \case
        NewPaymentError _ -> "TODO"
        NewPaymentTimeLimitReached _ -> "TODO"
        NewPaymentWalletIdDecodingFailed _ -> "TODO"
        NewPaymentUnknownAccountId _ -> "TODO"

instance ToServantError NewPaymentError where
    declareServantError = \case
        NewPaymentError _ -> err400 -- FIXME: Is this correct?
        NewPaymentTimeLimitReached _ -> err400 -- FIXME: Is this correct?
        NewPaymentWalletIdDecodingFailed _ -> err400 -- FIXME: Is this correct?
        NewPaymentUnknownAccountId _ -> err400 -- FIXME: Is this correct?

instance ToHttpErrorStatus NewPaymentError

instance ToJSON NewPaymentError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON NewPaymentError where
    parseJSON = jsendErrorGenericParseJSON

instance Exception NewPaymentError where
    toException   = walletExceptionToException
    fromException = walletExceptionFromException

instance Buildable NewPaymentError where
    build (NewPaymentError kernelErr) =
        bprint ("NewPaymentError " % build) kernelErr
    build (NewPaymentTimeLimitReached ter) =
        bprint ("NewPaymentTimeLimitReached " % build) ter
    build (NewPaymentWalletIdDecodingFailed txt) =
        bprint ("NewPaymentWalletIdDecodingFailed " % build) txt
    build (NewPaymentUnknownAccountId err) =
        bprint ("NewPaymentUnknownAccountId " % build) err

data EstimateFeesError =
      EstimateFeesError Kernel.EstimateFeesError
    | EstimateFeesTimeLimitReached TimeExecutionLimit
    | EstimateFeesWalletIdDecodingFailed Text
    deriving (Generic, Eq)

deriveGeneric ''EstimateFeesError

instance HasDiagnostic EstimateFeesError where
    getDiagnosticKey = \case
        EstimateFeesError _ -> "TODO"
        EstimateFeesTimeLimitReached _ -> "TODO"
        EstimateFeesWalletIdDecodingFailed _ -> "TODO"

instance ToServantError EstimateFeesError where
    declareServantError = \case
        EstimateFeesError _ -> err400 -- FIXME: Is this correct?
        EstimateFeesTimeLimitReached _ -> err400 -- FIXME: Is this correct?
        EstimateFeesWalletIdDecodingFailed _ -> err400 -- FIXME: Is this correct?

instance ToHttpErrorStatus EstimateFeesError

instance ToJSON EstimateFeesError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON EstimateFeesError where
    parseJSON = jsendErrorGenericParseJSON

-- | Unsound show instance needed for the 'Exception' instance.
instance Show EstimateFeesError where
    show = formatToString build

instance Exception EstimateFeesError where
    toException   = walletExceptionToException
    fromException = walletExceptionFromException

instance Buildable EstimateFeesError where
    build (EstimateFeesError kernelErr) =
        bprint ("EstimateFeesError " % build) kernelErr
    build (EstimateFeesTimeLimitReached ter) =
        bprint ("EstimateFeesTimeLimitReached " % build) ter
    build (EstimateFeesWalletIdDecodingFailed txt) =
        bprint ("EstimateFeesWalletIdDecodingFailed " % build) txt

data RedeemAdaError =
    RedeemAdaError Kernel.RedeemAdaError
  | RedeemAdaWalletIdDecodingFailed Text
  | RedeemAdaInvalidRedemptionCode InvalidRedemptionCode
  deriving (Generic, Eq)

deriveGeneric ''RedeemAdaError

instance HasDiagnostic RedeemAdaError where
    getDiagnosticKey = \case
        RedeemAdaError _ -> "TODO"
        RedeemAdaWalletIdDecodingFailed _ -> "TODO"
        RedeemAdaInvalidRedemptionCode _ -> "TODO"

instance ToServantError RedeemAdaError where
    declareServantError _ = err400 -- FIXME: Is this correct?

instance ToHttpErrorStatus RedeemAdaError

instance ToJSON RedeemAdaError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON RedeemAdaError where
    parseJSON = jsendErrorGenericParseJSON

-- | Unsound show instance needed for the 'Exception' instance.
instance Show RedeemAdaError where
    show = formatToString build

instance Exception RedeemAdaError where
    toException   = walletExceptionToException
    fromException = walletExceptionFromException

instance Buildable RedeemAdaError where
    build (RedeemAdaError err) =
        bprint ("RedeemAdaError " % build) err
    build (RedeemAdaWalletIdDecodingFailed txt) =
        bprint ("RedeemAdaWalletIdDecodingFailed " % build) txt
    build (RedeemAdaInvalidRedemptionCode txt) =
        bprint ("RedeemAdaInvalidRedemptionCode " % build) txt

------------------------------------------------------------
-- Passive wallet layer
------------------------------------------------------------

-- | The passive wallet (data) layer. See @PassiveWallet@.
data PassiveWalletLayer m = PassiveWalletLayer
    {
    -- wallets
      createWallet         :: NewWallet -> m (Either CreateWalletError Wallet)
    , getWallets           :: m (IxSet Wallet)
    , getWallet            :: WalletId -> m (Either GetWalletError Wallet)
    , updateWallet         :: WalletId
                           -> WalletUpdate
                           -> m (Either UpdateWalletError Wallet)
    , updateWalletPassword :: WalletId
                           -> PasswordUpdate
                           -> m (Either UpdateWalletPasswordError Wallet)
    , deleteWallet         :: WalletId -> m (Either DeleteWalletError ())
    , getUtxos             :: WalletId
                           -> m (Either GetUtxosError [(Account, Utxo)])
    -- accounts
    , createAccount        :: WalletId
                           -> NewAccount
                           -> m (Either CreateAccountError Account)
    , getAccounts          :: WalletId
                           -> m (Either GetAccountsError (IxSet Account))
    , getAccount           :: WalletId
                           -> AccountIndex
                           -> m (Either GetAccountError Account)
    , getAccountBalance    :: WalletId
                           -> AccountIndex
                           -> m (Either GetAccountError AccountBalance)
    , getAccountAddresses  :: WalletId
                           -> AccountIndex
                           -> RequestParams
                           -> FilterOperations '[V1 Address] WalletAddress
                           -> m (Either GetAccountError (WalletResponse [WalletAddress]))
    , updateAccount        :: WalletId
                           -> AccountIndex
                           -> AccountUpdate
                           -> m (Either UpdateAccountError Account)
    , deleteAccount        :: WalletId
                           -> AccountIndex
                           -> m (Either DeleteAccountError ())
    -- addresses
    , createAddress        :: NewAddress
                           -> m (Either CreateAddressError WalletAddress)
    , getAddresses         :: RequestParams -> m (SliceOf WalletAddress)
    , validateAddress      :: Text
                           -> m (Either ValidateAddressError WalletAddress)

    -- transactions
    , getTransactions      :: Maybe WalletId
                           -> Maybe AccountIndex
                           -> Maybe (V1 Address)
                           -> RequestParams
                           -> FilterOperations '[V1 TxId, V1 Timestamp] Transaction
                           -> SortOperations Transaction
                           -> m (Either GetTxError (WalletResponse [Transaction]))
    , getTxFromMeta        :: TxMeta -> m (Either Kernel.UnknownHdAccount Transaction)

    -- core API
    , applyBlocks          :: OldestFirst NE Blund -> m ()
    , rollbackBlocks       :: NewestFirst NE Blund -> m ()

    -- node settings
    , getNodeSettings      :: m NodeSettings

    -- internal
    , nextUpdate           :: m (Maybe (V1 SoftwareVersion))
    , applyUpdate          :: m ()
    , postponeUpdate       :: m ()
    , resetWalletState     :: m ()
    }

------------------------------------------------------------
-- Active wallet layer
------------------------------------------------------------

-- An active wallet layer. See @ActiveWallet@.
data ActiveWalletLayer m = ActiveWalletLayer {
      -- | The underlying passive wallet layer
      walletPassiveLayer :: PassiveWalletLayer m

      -- | Performs a payment.
    , pay :: PassPhrase
          -- The \"spending password\" to decrypt the 'EncryptedSecretKey'.
          -> InputGrouping
          -- An preference on how to group inputs during coin selection.
          -> ExpenseRegulation
          -- Who pays the fee, if the sender or the receivers.
          -> Payment
          -- The payment we need to perform.
          -> m (Either NewPaymentError (Tx, TxMeta))

      -- | Estimates the fees for a payment.
    , estimateFees :: PassPhrase
                   -- The \"spending password\" to decrypt the 'EncryptedSecretKey'.
                   -> InputGrouping
                   -- An preference on how to group inputs during coin selection
                   -> ExpenseRegulation
                   -- Who pays the fee, if the sender or the receivers.
                   -> Payment
                   -- The payment we need to perform.
                   -> m (Either EstimateFeesError Coin)

      -- | Redeem ada
    , redeemAda :: Redemption -> m (Either RedeemAdaError (Tx, TxMeta))

      -- | Node info
      --
      -- This lives in the active wallet layer as the node info endpoint returns
      -- status information about the diffusion layer
    , getNodeInfo :: ForceNtpCheck -> m NodeInfo
    }
