-- | Append-only SQLite persistence under an exclusive connection lock.
module MyOrg.Infrastructure.SQLiteStore
  ( openSQLitePersistence
  ) where

import Control.Exception (onException)
import Control.Monad (unless)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Database.SQLite.Simple
import MyOrg.Application.Persistence
import MyOrg.Domain.Event.Types
import MyOrg.Serialization.JSON (eitherDecodeWire, encodeWire)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

openSQLitePersistence :: FilePath -> IO Persistence
openSQLitePersistence path = do
  unless (not (null path) && path /= ":memory:") $
    ioError (userError "MY_ORG_SQLITE_FILE must name a persistent SQLite file")
  createDirectoryIfMissing True (takeDirectory path)
  connection <- open path
  ( do
      initialize connection
      rows <- query_ connection "SELECT sequence, payload FROM my_org_events ORDER BY sequence"
      events <- mapM decodeEvent rows
      pure (Persistence events (persist connection) (close connection))
    )
    `onException` close connection

initialize :: Connection -> IO ()
initialize connection = do
  modes <- query_ connection "PRAGMA locking_mode = EXCLUSIVE" :: IO [Only Text]
  unless (modes == [Only "exclusive"]) $
    ioError (userError "SQLite exclusive locking is required")
  journals <- query_ connection "PRAGMA journal_mode = DELETE" :: IO [Only Text]
  unless (journals == [Only "delete"]) $
    ioError (userError "SQLite rollback journaling is required")
  execute_ connection "PRAGMA synchronous = FULL"
  -- Acquire the lock even when the table already exists. EXCLUSIVE locking mode
  -- retains it after commit, until this connection closes.
  withExclusiveTransaction connection $
    execute_
      connection
      "CREATE TABLE IF NOT EXISTS my_org_events (sequence INTEGER PRIMARY KEY, payload TEXT NOT NULL)"

decodeEvent :: (Int, Text) -> IO StoredEvent
decodeEvent (sequenceNumber, payload) = do
  event <-
    either (ioError . userError . ("Corrupt SQLite event: " <>)) pure $
      eitherDecodeWire (BL.fromStrict (TE.encodeUtf8 payload))
  unless (storedSeq event == sequenceNumber) $
    ioError (userError "Corrupt SQLite event sequence; store was not changed")
  pure event

persist :: Connection -> [StoredEvent] -> IO ()
persist connection events = withTransaction connection $ do
  counts <- query_ connection "SELECT COUNT(*) FROM my_org_events" :: IO [Only Int]
  count <- case counts of
    [Only n] -> pure n
    _        -> ioError (userError "Could not read SQLite event count")
  executeMany
    connection
    "INSERT INTO my_org_events(sequence,payload) VALUES (?,?)"
    [ (storedSeq event, TE.decodeUtf8 (BL.toStrict (encodeWire event)))
    | event <- drop count events
    ]
