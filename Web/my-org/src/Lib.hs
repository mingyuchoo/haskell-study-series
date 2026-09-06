module Lib
  ( someFunc
  ) where

import Control.Exception (bracket)
import Data.ByteString.Char8 qualified as BS
import Data.Map.Strict qualified as Map
import MyOrg.Demo (isDemoStore)
import MyOrg.Registry (registryEvents)
import MyOrg.Server (application)
import MyOrg.Store
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

someFunc :: IO ()
someFunc = do
  demo <- (== Just "1") <$> lookupEnv "MY_ORG_DEMO"
  file <- maybe "runs/local/events.json" id <$> lookupEnv "MY_ORG_EVENT_FILE"
  port <- maybe 8080 id . (>>= readMaybe) <$> lookupEnv "MY_ORG_PORT"
  database <- lookupEnv "MY_ORG_TEST_DATABASE_URL"
  let chosenFile = if demo then "runs/demo/events.json" else file
      chosenPort = if demo then 8081 else port
      open =
        if demo
          then openFileStore chosenFile
          else maybe (openFileStore chosenFile) (openPostgresStore . BS.pack) database
  putStrLn
    ("My Org: http://127.0.0.1:" <> show chosenPort <> " (local, unauthenticated MVP)")
  bracket open closeStore $ \store -> do
    if demo
      then do
        events <- readAudit store
        registry <- readRegistry store
        if null events
          then do
            result <- seedDemo store
            either
              (const (ioError (userError "Demo initialization failed; existing data was not changed")))
              (const (pure ()))
              result
          else
            if any isDemoStore (Map.elems (registryEvents registry))
              then putStrLn "Continuing the saved demo workspace; your changes are preserved."
              else
                ioError (userError "The demo path contains another workspace; refusing to change it.")
      else pure ()
    runSettings (setPort chosenPort (setHost "127.0.0.1" defaultSettings)) (application store)
