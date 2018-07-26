module JSONLog
    ( jsonLogs
    , parseLogP
    , IndexedJLTimedEvent (..)
    , runParseLogs
    ) where

import           Universum

import           Conduit (MonadResource)
import           Data.Attoparsec.Text (Parser, parseOnly, takeTill)
import           Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit.Combinators as C
import           Data.Conduit.TMChan (mergeSources)
-- import           Pipes
-- import           Pipes.ByteString (fromHandle)
-- import           Pipes.Interleave (interleave)
-- import qualified Pipes.Prelude as P
import           System.Directory (listDirectory)
import           System.FilePath ((</>))
import           UnliftIO (MonadUnliftIO)

import           Pos.Infra.Util.JsonLog.Events (JLEvent, JLTimedEvent (..))
import           Types
import           Util.Aeson (parseJSONP)

jsonLogs :: MonadIO m => FilePath -> m [(NodeId, FilePath)]
jsonLogs logDir = do
    files <- liftIO (listDirectory logDir)
    return $ map (second (logDir </>)) $ mapMaybe f files
  where
    f :: FilePath -> Maybe (NodeId, FilePath)
    f logFile = case parseOnly nodeIndexParser $ toText logFile of
        Right name -> Just (name, logFile)
        Left _     -> Nothing

nodeIndexParser :: Parser NodeId
nodeIndexParser = takeTill (== '.') <* ".json"

parseLogP :: MonadResource m => FilePath -> ConduitT () JLTimedEvent m ()
parseLogP fp = C.sourceFile fp .| parseJSONP

data IndexedJLTimedEvent = IndexedJLTimedEvent
    { ijlNode      :: !NodeId
    , ijlTimestamp :: !Timestamp
    , ijlEvent     :: !JLEvent
    }

instance Eq IndexedJLTimedEvent where

    (==) = (==) `on` ijlTimestamp

instance Ord IndexedJLTimedEvent where

    compare = compare `on` ijlTimestamp

runParseLogs :: forall m r.
                (MonadResource m, MonadUnliftIO m)
             => FilePath -> (ConduitT () IndexedJLTimedEvent m () -> m r)
             -> m r
runParseLogs logDir f = do
    xs <- jsonLogs logDir
    sources <- mergeSources (map (uncurry producer) xs) 20
    f sources
  where
    producer :: NodeId -> FilePath -> ConduitT () IndexedJLTimedEvent m ()
    producer n fp = parseLogP fp .| C.map (\JLTimedEvent{..} ->
        IndexedJLTimedEvent { ijlNode      = n
                            , ijlTimestamp = fromIntegral jlTimestamp
                            , ijlEvent     = jlEvent
                            })
