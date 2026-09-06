module SQLiteStoreSpec
  ( spec
  ) where

import Control.Exception (bracket)
import Data.ByteString.Lazy qualified as BL
import Data.Either (isRight)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Database.SQLite.Simple
import MyOrg.Domain.Event
import MyOrg.Serialization.JSON (encodeWire)
import MyOrg.Store
import MyOrg.Types
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "SQLite event persistence" $ do
  it "creates parent directories and restores the complete audit and projection" $ withDatabase $ \path -> do
    saved <- bracket (openSQLiteStore path) closeStore $ \store -> do
      seedDemo store >>= (`shouldSatisfy` isRight)
      readStore store
    bracket (openSQLiteStore path) closeStore $ \store -> readStore store `shouldReturn` saved
  it "rejects a second server and releases the lock when closed" $ withDatabase $ \path -> do
    bracket (openSQLiteStore path) closeStore $ \_ ->
      bracket (openSQLiteStore path) closeStore (const (pure ())) `shouldThrow` anyException
    bracket (openSQLiteStore path) closeStore $ \store -> readAudit store `shouldReturn` []
  it "converts an existing WAL database to DELETE journal mode before locking" $ withDatabase $ \path -> do
    initialize path
    withConnection path $ \connection ->
      query_ connection "PRAGMA journal_mode=WAL" `shouldReturn` [Only ("wal" :: Text)]
    bracket (openSQLiteStore path) closeStore $ \_ ->
      bracket (openSQLiteStore path) closeStore (const (pure ())) `shouldThrow` anyException
    withConnection path $ \connection ->
      query_ connection "PRAGMA journal_mode" `shouldReturn` [Only ("delete" :: Text)]
  it "rejects a table sequence that differs from the encoded event sequence" $ withDatabase $ \path -> do
    saved <- bracket (openSQLiteStore path) closeStore $ \store -> do
      seedDemo store >>= (`shouldSatisfy` isRight)
      readAudit store
    withConnection path $ \connection ->
      execute
        connection
        "UPDATE my_org_events SET sequence = ? WHERE sequence = 1"
        (Only (length saved + 1))
    bracket (openSQLiteStore path) closeStore (const (pure ())) `shouldThrow` anyException
    withConnection path $ \connection ->
      query_ connection "SELECT COUNT(*) FROM my_org_events"
        `shouldReturn` [Only (length saved)]
  it "rolls back an entire failed batch and preserves runtime memory" $ withDatabase $ \path -> do
    initialize path
    withConnection path $ \connection ->
      execute_
        connection
        "CREATE TRIGGER reject_second_event BEFORE INSERT ON my_org_events WHEN NEW.sequence = 2 BEGIN SELECT RAISE(ABORT, 'test write failure'); END"
    bracket (openSQLiteStore path) closeStore $ \store -> do
      savedBefore <- readStore store
      seedDemo store `shouldReturn` Left StorageFailure
      readStore store `shouldReturn` savedBefore
    withConnection path $ \connection -> do
      query_ connection "SELECT COUNT(*) FROM my_org_events" `shouldReturn` [Only (0 :: Int)]
      execute_ connection "DROP TRIGGER reject_second_event"
    bracket (openSQLiteStore path) closeStore $ \store -> seedDemo store >>= (`shouldSatisfy` isRight)
  it "preserves corrupt payloads and releases the lock after decoding fails" $ withDatabase $ \path -> do
    initialize path
    withConnection path $ \connection ->
      execute
        connection
        "INSERT INTO my_org_events VALUES (?, ?)"
        (1 :: Int, "{corrupt" :: Text)
    bracket (openSQLiteStore path) closeStore (const (pure ())) `shouldThrow` anyException
    withConnection path $ \connection -> do
      query_ connection "SELECT payload FROM my_org_events"
        `shouldReturn` [Only ("{corrupt" :: Text)]
      execute_ connection "DELETE FROM my_org_events"
    bracket (openSQLiteStore path) closeStore $ \store -> readAudit store `shouldReturn` []
  it "rejects discontinuous event sequences and releases the runtime initialization lock" $ withDatabase $ \path -> do
    initialize path
    let timestamp = read "2026-01-01 00:00:00 UTC"
        event =
          StoredEvent
            2
            timestamp
            Nothing
            (OrganizationCreated (Organization (OrgId "org") "조직" timestamp))
    withConnection path $ \connection ->
      execute
        connection
        "INSERT INTO my_org_events VALUES (?, ?)"
        (2 :: Int, TE.decodeUtf8 (BL.toStrict (encodeWire event)))
    bracket (openSQLiteStore path) closeStore (const (pure ())) `shouldThrow` anyException
    withConnection path $ \connection -> do
      query_ connection "SELECT sequence FROM my_org_events" `shouldReturn` [Only (2 :: Int)]
      execute_ connection "DELETE FROM my_org_events"
    bracket (openSQLiteStore path) closeStore $ \store -> readAudit store `shouldReturn` []

withDatabase :: (FilePath -> IO a) -> IO a
withDatabase action = withSystemTempDirectory "my-org-sqlite-" $ \directory ->
  action (directory </> "nested" </> "events.sqlite")

initialize :: FilePath -> IO ()
initialize path = bracket (openSQLiteStore path) closeStore (const (pure ()))
