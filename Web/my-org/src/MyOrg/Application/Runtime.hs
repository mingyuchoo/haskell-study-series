-- | Serializes clock, pure planning, durable write and state publication.
module MyOrg.Application.Runtime
  ( Store, openStore, closeStore, readStore, readRegistry, readAudit
  , runCommand, runOrganizationCommand, seedDemo
  ) where

import Control.Concurrent.MVar
import Control.Exception (SomeException, AsyncException, fromException, throwIO, try, onException)
import Control.Monad (unless)
import Data.Time (getCurrentTime)
import qualified Data.Map.Strict as Map
import MyOrg.Domain.Identity
import MyOrg.Domain.Error
import MyOrg.Application
import MyOrg.Application.Plan
import MyOrg.Application.Persistence
import MyOrg.Registry
import MyOrg.Domain.Event.Types
import MyOrg.Domain.State

data Store = Store (MVar [StoredEvent]) ([StoredEvent] -> IO ()) (IO ())

-- A failed initialization releases the adapter's file/database lock.
openStore :: Persistence -> IO Store
openStore backend = (do
  let events = initialEvents backend
  validateSequence events
  _ <- requireRegistry events
  var <- newMVar events
  pure (Store var (persistEvents backend) (releasePersistence backend)))
  `onException` releasePersistence backend

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

-- Planning must remain inside the same lock as reading and persistence.
runSelected :: Store -> Maybe OrgId -> Maybe UserId -> Command -> IO (Either OrganizationError [StoredEvent])
runSelected (Store var persist _) selected actor command = modifyMVar var $ \events -> do
  now <- getCurrentTime
  commitEvents persist events (planCommand now events selected actor command)

seedDemo :: Store -> IO (Either OrganizationError [StoredEvent])
seedDemo (Store var persist _) = modifyMVar var $ \events -> do
  now <- getCurrentTime
  commitEvents persist events (planDemo now events)

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
