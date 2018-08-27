module Pos.Util.Wlog
        ( module System.Wlog
        , module System.Wlog.LogHandler
        , module System.Wlog.Formatter

        , logExample
        , logExampleLog
        , logExampleWlog

        ) where

import           System.Wlog (CanLog (..), HandlerWrap (..), HasLoggerName (..),
                     LogEvent (..), LoggerConfig (..), LoggerName (..),
                     LoggerNameBox (..), NamedPureLogger (..), Severity (..),
                     WithLogger, consoleActionB, debugPlus,
                     defaultHandleAction, dispatchEvents, errorPlus,
                     fromScratch, hwFilePath, infoPlus, launchNamedPureLog,
                     lcLogsDirectory, lcTermSeverityOut, lcTree, logDebug,
                     logError, logInfo, logMCond, logMessage, logNotice,
                     logWarning, ltFiles, ltSeverity, ltSubloggers,
                     maybeLogsDirB, modifyLoggerName, noticePlus,
                     parseLoggerConfig, productionB, removeAllHandlers,
                     retrieveLogContent, runNamedPureLog, setLevel,
                     setupLogging, showTidB, termSeveritiesOutB,
                     updateGlobalLogger, usingLoggerName, warningPlus,
                     zoomLogger)
import           System.Wlog.Formatter (centiUtcTimeF)
import           System.Wlog.LogHandler (LogHandlerTag (HandlerFilelike))

import qualified Pos.Util.Log as Log
import qualified Pos.Util.Log.LoggerConfig as LoggerConfig
import           Universum

logExampleWlog :: (MonadIO m, CanLog m) => m ()
logExampleWlog = do
    setupLogging Nothing $ productionB
    usingLoggerName "logExample" $ do
        logInfo  "Processing command (INFO)"
        logDebug "Processing command (DEBUG)"

-- logExampleLog :: (MonadIO m, Log.CanLog m) => m ()
logExampleLog :: (Log.WithLogger m) => m ()
logExampleLog = do
    lh <- Log.setupLogging $ LoggerConfig.defaultInteractiveConfiguration Log.Debug
    liftIO $ Log.usingLoggerName lh "logExample" $ do
        Log.logInfo  "Processing command (INFO)"
        Log.logDebug "Processing command (DEBUG)"

logExample :: IO ()
logExample = do
    lh <- Log.setupLogging $ LoggerConfig.defaultInteractiveConfiguration Log.Debug
    Log.usingLoggerName lh "logExample" $ do
        Log.logInfo  "Processing command (INFO)"
        Log.logDebug "Processing command (DEBUG)"
