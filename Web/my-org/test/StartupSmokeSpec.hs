module StartupSmokeSpec
  ( spec
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, try)
import Control.Monad (void, when)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List (isInfixOf)
import Data.Text qualified as T
import MyOrg.Serialization.JSON (eitherDecodeWire)
import Network.HTTP.Client (HttpException)
import Network.Socket (close)
import Network.Wai.Handler.Warp (openFreePort)
import SmokeSupport
import System.Directory (createDirectoryLink, getCurrentDirectory, renameFile)
import System.Environment (getEnvironment, getExecutablePath)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (IOMode (..), withFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "Process startup and persisted demo lifecycle" $
  it "seeds, preserves deletion/replacement, and refuses unrelated data without mutation" $
    withSystemTempDirectory "my-org-startup-" $ \directory -> do
      root <- getCurrentDirectory
      createDirectoryLink (root </> "static") (directory </> "static")
      binary <- getExecutablePath
      inherited <- getEnvironment
      let eventFile = directory </> "runs/demo/events.json"
          cleanEnv =
            filter
              ( \(key, _) ->
                  key
                    `notElem` ["MY_ORG_TEST_DATABASE_URL", "MY_ORG_DEMO", "MY_ORG_EVENT_FILE", "MY_ORG_PORT"]
              )
              inherited
          config demo port =
            (proc binary ["--startup-server", "+RTS", "-N2", "-RTS"])
              { cwd = Just directory
              , create_group = True
              , env =
                  Just
                    ( ("MY_ORG_EVENT_FILE", eventFile)
                        : ("MY_ORG_PORT", show port)
                        : [("MY_ORG_DEMO", "1") | demo] <> cleanEnv
                    )
              }
          bootstrap continuing = do
            -- Production demo mode always uses 8081. Never send HTTP to that port.
            -- A bind conflict is acceptable only after the store/startup branch succeeds.
            (exitCode, output) <- runBootstrap directory (config True (0 :: Int))
            output `shouldSatisfy` (not . isInfixOf "refusing")
            output `shouldSatisfy` (not . isInfixOf "initialization failed")
            when continuing $
              output
                `shouldSatisfy` isInfixOf "Continuing the saved demo workspace; your changes are preserved."
            (exitCode, output)
              `shouldSatisfy` ( \(code, logText) ->
                                  code == Nothing
                                    || code == Just ExitSuccess
                                    || "Address already in use" `isInfixOf` logText
                              )
          serve action = do
            (port, socket) <- openFreePort
            close socket
            withFile (directory </> "server.log") WriteMode $ \logHandle ->
              bracket
                ( createProcess
                    (config False port) {std_out = UseHandle logHandle, std_err = UseHandle logHandle}
                )
                stop
                $ \(_, _, _, process) ->
                  withClient port $ \client -> do
                    waitReady client process 100
                    action client
      bootstrap False
      bytes <- BS.readFile eventFile
      seeded <- either fail pure (eitherDecodeWire (BL.fromStrict bytes))
      length (items seeded) `shouldSatisfy` (> 50)
      serve $ \client -> do
        state <- get client "dashboard"
        let org = field state "organization"
            identifier = case field org "id" of String text -> T.unpack text; value -> error (show value)
        _ <-
          call
            client
            "DELETE"
            ("organizations/" <> identifier)
            ( Just
                (object ["confirmName" .= field org "name", "expectedVersion" .= field state "version"])
            )
            200
        deleted <- get client "dashboard"
        field deleted "organization" `shouldBe` Null
      deletedBytes <- BS.readFile eventFile
      bootstrap True
      BS.readFile eventFile `shouldReturn` deletedBytes
      serve $ \client -> do
        state <- get client "dashboard"
        field state "organization" `shouldBe` Null
        _ <-
          post
            client
            "organizations"
            (object ["id" .= String "new-local-org", "name" .= String "가상의 재생성 조직"])
            201
        void
          ( post
              client
              "people"
              (object ["id" .= String "new-person", "name" .= String "새 가상 인물", "role" .= String "담당자"])
              201
          )
      recreatedBytes <- BS.readFile eventFile
      bootstrap True
      BS.readFile eventFile `shouldReturn` recreatedBytes
      serve $ \client -> do
        state <- get client "dashboard"
        field (field state "organization") "id" `shouldBe` String "new-local-org"
        length (items (field state "people")) `shouldBe` 1
        field state "demo" `shouldBe` Bool False
      renameFile eventFile (directory </> "previous-lifecycle-audit.json")
      serve $ \client ->
        void
          ( post
              client
              "organizations"
              (object ["id" .= String "unrelated", "name" .= String "기존 별도 가상 조직"])
              201
          )
      unrelatedBytes <- BS.readFile eventFile
      (exitCode, output) <- runBootstrap directory (config True (0 :: Int))
      exitCode `shouldSatisfy` maybe False (/= ExitSuccess)
      output `shouldSatisfy` isInfixOf "refusing to change"
      BS.readFile eventFile `shouldReturn` unrelatedBytes

runBootstrap :: FilePath -> CreateProcess -> IO (Maybe ExitCode, String)
runBootstrap directory config = do
  let logFile = directory </> "bootstrap.log"
  code <- withFile logFile WriteMode $ \handle ->
    bracket
      (createProcess config {std_out = UseHandle handle, std_err = UseHandle handle})
      stop
      $ \(_, _, _, process) ->
        timeout 1500000 (waitForProcess process)
  output <- readFile logFile
  -- Force lazy IO before the next startup overwrites this log.
  length output `seq` pure (code, output)

stop :: (Maybe a, Maybe b, Maybe c, ProcessHandle) -> IO ()
stop (_, _, _, process) = do
  running <- getProcessExitCode process
  case running of
    Just _ -> pure ()
    Nothing -> do
      interruptProcessGroupOf process
      stopped <- timeout 5000000 (waitForProcess process)
      case stopped of
        Just _ -> pure ()
        Nothing -> do
          terminateProcess process
          terminated <- timeout 2000000 (waitForProcess process)
          case terminated of
            Just _ -> pure ()
            Nothing -> do
              pid <- getPid process
              mapM_ (signalProcess sigKILL) pid
              void (waitForProcess process)

waitReady :: Client -> ProcessHandle -> Int -> IO ()
waitReady _ _ 0 = fail "Disposable startup server did not become ready"
waitReady client process attempts = do
  running <- getProcessExitCode process
  case running of
    Just code -> fail ("Disposable startup server exited: " <> show code)
    Nothing -> do
      response <-
        try (request client "GET" "dashboard" Nothing) :: IO (Either HttpException (Int, Value))
      case response of
        Right (200, _) -> pure ()
        _              -> threadDelay 50000 >> waitReady client process (attempts - 1)
