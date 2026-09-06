-- | Append-only PostgreSQL persistence under an advisory session lock.
module MyOrg.Infrastructure.PostgresStore
  ( openPostgresPersistence
  ) where

import Control.Exception (onException)
import Control.Monad (unless, void)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Database.PostgreSQL.Simple
import MyOrg.Application.Persistence
import MyOrg.Domain.Event.Types
import MyOrg.Serialization.JSON (eitherDecodeWire, encodeWire)

openPostgresPersistence :: BS.ByteString -> IO Persistence
openPostgresPersistence connectionString = do
  connection <- connectPostgreSQL connectionString
  ( do
      -- This adapter is only selected explicitly by the local operator.
      locked <- query_ connection "SELECT pg_try_advisory_lock(714260901)" :: IO [Only Bool]
      unless
        (locked == [Only True])
        (ioError (userError "Another my-org server holds the database lock"))
      void $
        execute_
          connection
          "CREATE TABLE IF NOT EXISTS my_org_events (sequence BIGINT PRIMARY KEY, payload TEXT NOT NULL)"
      rows <-
        query_ connection "SELECT payload FROM my_org_events ORDER BY sequence" :: IO [Only Text]
      events <-
        mapM
          ( \(Only payload) ->
              either
                (ioError . userError)
                pure
                (eitherDecodeWire (BL.fromStrict (TE.encodeUtf8 payload)))
          )
          rows
      let persist xs = withTransaction connection $ do
            counts <- query_ connection "SELECT COUNT(*) FROM my_org_events" :: IO [Only Int]
            let count = case counts of [Only n] -> n; _ -> 0
            mapM_
              ( \event ->
                  void $
                    execute
                      connection
                      "INSERT INTO my_org_events(sequence,payload) VALUES (?,?)"
                      (storedSeq event, TE.decodeUtf8 (BL.toStrict (encodeWire event)))
              )
              (drop count xs)
      pure (Persistence events persist (close connection))
    )
    `onException` close connection
