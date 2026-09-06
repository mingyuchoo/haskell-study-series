module MyOrg.Store (Store, openFileStore, openPostgresStore, closeStore, readStore, readRegistry, readAudit, runCommand, runOrganizationCommand, seedDemo) where

import Control.Concurrent.MVar
import Control.Exception (SomeException, AsyncException, fromException, throwIO, try, bracketOnError, onException)
import Control.Monad (unless, void)
import Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import qualified Data.Map.Strict as Map
import Database.PostgreSQL.Simple
import System.Directory
import System.IO (openBinaryTempFile, hClose)
import MyOrg.Types
import MyOrg.Demo (demoEvents, demoOrganizationId)
import MyOrg.Registry
import MyOrg.Application
import MyOrg.Domain.Event

data Store = Store (MVar [StoredEvent]) ([StoredEvent] -> IO ()) (IO ())

-- Atomic replacement: persistence completes before the in-memory state changes.
-- A directory lock prevents two local servers from sharing the same event file.
openFileStore :: FilePath -> IO Store
openFileStore path = do
  let dir = case reverse (dropWhile (/='/') (reverse path)) of "" -> "."; d -> d
      lock = path <> ".lock"
  createDirectoryIfMissing True dir
  createDirectory lock
  (do
    exists <- doesFileExist path
    events <- if exists then BL.readFile path >>= decodeEvents else pure []
    validateSequence events
    _ <- requireRegistry events
    var <- newMVar events
    let persist xs = bracketOnError (openBinaryTempFile dir ".my-org-events.tmp") (\(tmp,h) -> hClose h >> removeFile tmp) $ \(tmp,h) -> do
          BL.hPut h (encode xs)
          hClose h
          renameFile tmp path
    pure (Store var persist (removeDirectory lock))) `onException` removeDirectory lock

openPostgresStore :: BS.ByteString -> IO Store
openPostgresStore connectionString = do
  connection <- connectPostgreSQL connectionString
  (do
    -- This adapter is only selected explicitly by the local operator.
    locked <- query_ connection "SELECT pg_try_advisory_lock(714260901)" :: IO [Only Bool]
    unless (locked == [Only True]) (ioError (userError "Another my-org server holds the database lock"))
    void $ execute_ connection "CREATE TABLE IF NOT EXISTS my_org_events (sequence BIGINT PRIMARY KEY, payload TEXT NOT NULL)"
    rows <- query_ connection "SELECT payload FROM my_org_events ORDER BY sequence" :: IO [Only Text]
    events <- mapM (\(Only payload) -> either (ioError . userError) pure (eitherDecode (BL.fromStrict (TE.encodeUtf8 payload)))) rows
    validateSequence events
    _ <- requireRegistry events
    var <- newMVar events
    let persist xs = withTransaction connection $ do
          counts <- query_ connection "SELECT COUNT(*) FROM my_org_events" :: IO [Only Int]
          let count = case counts of [Only n] -> n; _ -> 0
          mapM_ (\event -> void $ execute connection "INSERT INTO my_org_events(sequence,payload) VALUES (?,?)" (storedSeq event, TE.decodeUtf8 (BL.toStrict (encode event)))) (drop count xs)
    pure (Store var persist (close connection))) `onException` close connection

decodeEvents :: BL.ByteString -> IO [StoredEvent]
decodeEvents bytes = either (ioError . userError . ("Corrupt event file: " <>)) pure (eitherDecode bytes)

validateSequence :: [StoredEvent] -> IO ()
validateSequence events = unless (map storedSeq events == [1 .. length events]) (ioError (userError "Corrupt event sequence; store was not changed"))

closeStore :: Store -> IO ()
closeStore (Store _ _ cleanup) = cleanup

readAudit :: Store -> IO [StoredEvent]
readAudit (Store var _ _) = readMVar var

readRegistry :: Store -> IO Registry
readRegistry store = readAudit store >>= requireRegistry

requireRegistry :: [StoredEvent] -> IO Registry
requireRegistry = either (ioError . userError . show) pure . replayRegistry

-- Compatibility for domain clients that intentionally operate one active org.
-- Multiple active organizations are never selected implicitly.
readStore :: Store -> IO (OrgState, [StoredEvent])
readStore (Store var _ _) = withMVar var $ \events -> do
  registry <- requireRegistry events
  case resolveSingleOrganization registry of
    Left err -> ioError (userError (show err))
    Right Nothing -> pure (emptyState {stateLastSeq = registryLastSeq registry}, events)
    Right (Just oid) -> case organizationState registry oid of
      Left err -> ioError (userError (show err))
      Right st -> pure (st, Map.findWithDefault [] oid (registryEvents registry))

runCommand :: Store -> Maybe UserId -> Command -> IO (Either OrganizationError [StoredEvent])
runCommand store actor command = runSelected store Nothing actor command

runOrganizationCommand :: Store -> OrgId -> Maybe UserId -> Command -> IO (Either OrganizationError [StoredEvent])
runOrganizationCommand store oid = runSelected store (Just oid)

runSelected :: Store -> Maybe OrgId -> Maybe UserId -> Command -> IO (Either OrganizationError [StoredEvent])
runSelected (Store var persist _) selected actor command = modifyMVar var $ \events -> do
  now <- getCurrentTime
  let checked = do
        registry <- replayRegistry events
        oid <- commandScope registry selected command
        st <- case command of
          CreateOrganization _ _ -> pure (Map.findWithDefault emptyState oid (registryStates registry))
          _ -> organizationState registry oid
        mapM_ (\uid -> unless (Map.member uid (statePeople st)) (Left (PersonNotFound uid))) actor
        changes <- executeCommand now st command
        let additions = zipWith (\n event -> StoredEvent n now actor (OrganizationScoped oid event)) [length events + 1 ..] changes
        _ <- replayRegistry (events <> additions)
        pure additions
  commitEvents persist events checked

commandScope :: Registry -> Maybe OrgId -> Command -> Either OrganizationError OrgId
commandScope registry selected command = do
  let intrinsic = case command of
        CreateOrganization oid _ -> Just oid
        RenameOrganization oid _ _ -> Just oid
        DeleteOrganization oid _ _ -> Just oid
        _ -> Nothing
  case (selected, intrinsic) of
    (Just target, Just inner) | target /= inner -> Left (InvalidInput "명령의 조직과 선택한 조직이 일치하지 않습니다.")
    (Just target, _) -> pure target
    (_, Just target) -> pure target
    _ -> resolveSingleOrganization registry >>= maybe (Left NoOrganization) Right

-- The entire batch is checked and committed under the same global store lock.
-- It may coexist with other organizations; only its active organization ID is unique.
seedDemo :: Store -> IO (Either OrganizationError [StoredEvent])
seedDemo (Store var persist _) = modifyMVar var $ \events -> do
  now <- getCurrentTime
  let checked = do
        registry <- replayRegistry events
        case organizationState registry demoOrganizationId of
          Right _ -> Left OrganizationAlreadyExists
          Left _ -> Right ()
        generated <- demoEvents now
        let additions = map (\event -> event
              { storedSeq = storedSeq event + length events
              , storedEvent = OrganizationScoped demoOrganizationId (storedEvent event)
              }) generated
        _ <- replayRegistry (events <> additions)
        pure additions
  commitEvents persist events checked

commitEvents :: ([StoredEvent] -> IO ()) -> [StoredEvent] -> Either OrganizationError [StoredEvent] -> IO ([StoredEvent], Either OrganizationError [StoredEvent])
commitEvents _ events (Left err) = pure (events, Left err)
commitEvents persist events (Right additions) = do
  let next = events <> additions
  saved <- try (persist next) :: IO (Either SomeException ())
  case saved of
    Left err -> case fromException err :: Maybe AsyncException of
      Just interrupt -> throwIO interrupt
      Nothing -> pure (events, Left StorageFailure)
    Right () -> pure (next, Right additions)
