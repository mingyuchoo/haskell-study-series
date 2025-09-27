module TestUtils
    where

import           Control.Concurrent          (ThreadId, forkIO, threadDelay)
import           Control.Monad.Logger        (runStdoutLoggingT)
import           Control.Monad.Reader        (runReaderT)

import           DB.Basic                    (PGInfo, localConnString)
import           Schema.Cache               (RedisInfo, localRedisInfo)

import           Database.Persist.Postgresql (runMigrationSilent,
                                              withPostgresqlConn)
import           Database.Persist            (deleteWhere, Filter)
import           Database.Redis              (connect, runRedis, flushall)

import           Network.HTTP.Client         (newManager)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)

import           Schema.Basic                (migrateAll, User)

import           Servant.Client              (ClientEnv, mkClientEnv,
                                              parseBaseUrl)

import           Server.Cache                (runServer, rootApiListClient)
import           Servant.Client              (runClientM)

setupTests :: IO (PGInfo, RedisInfo, ClientEnv, ThreadId)
setupTests = do
  mgr <- newManager tlsManagerSettings
  baseUrl <- parseBaseUrl "http://127.0.0.1:8000"
  let clientEnv = mkClientEnv mgr baseUrl
  -- Ensure DB schema exists and start from a clean slate
  _ <- runStdoutLoggingT $ withPostgresqlConn localConnString $ \dbConn -> do
    -- run migrations
    _ <- runReaderT (runMigrationSilent migrateAll) dbConn
    -- delete all existing users to ensure empty state for tests
    runReaderT (deleteWhere ([] :: [Filter User])) dbConn
  -- Flush Redis cache to ensure empty cache
  redisConn <- connect localRedisInfo
  _ <- runRedis redisConn flushall
  tid <- forkIO runServer
  -- Wait until the server is actually ready instead of a fixed delay
  waitUntilServerReady clientEnv 30 -- up to ~3s
  return (localConnString, localRedisInfo, clientEnv, tid)

-- Poll the root endpoint until the server responds or retries run out
waitUntilServerReady :: ClientEnv -> Int -> IO ()
waitUntilServerReady _ 0 = threadDelay 100000 -- final short wait
waitUntilServerReady clientEnv retries = do
  res <- runClientM rootApiListClient clientEnv
  case res of
    Right _ -> pure ()
    Left _  -> do
      threadDelay 100000 -- 100ms
      waitUntilServerReady clientEnv (retries - 1)


