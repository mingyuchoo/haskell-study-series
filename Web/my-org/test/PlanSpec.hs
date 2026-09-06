module PlanSpec
  ( spec
  ) where

import Control.Exception (AsyncException (..), IOException, throwIO, try)
import Data.Either (isLeft)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime)
import MyOrg.Application
import MyOrg.Application.Persistence
import MyOrg.Application.Plan
import MyOrg.Application.Runtime qualified as Runtime
import MyOrg.Domain.Event
import MyOrg.Registry
import MyOrg.Types
import Test.Hspec

spec :: Spec
spec = describe "pure command planning" $ do
  it "uses supplied time, actor and global sequence while preserving organization scope" $ do
    let command = AddPerson (Person (UserId "new") "New" "Role" Nothing)
        expected =
          StoredEvent
            4
            later
            (Just uid)
            (OrganizationScoped aid (PersonAdded (Person (UserId "new") "New" "Role" Nothing)))
    planCommand later history (Just aid) (Just uid) command `shouldBe` Right [expected]
  it "rejects cross-scope actors, intrinsic scope mismatch and ambiguous legacy commands" $ do
    planCommand later history (Just bid) (Just uid) (RenameOrganization bid "B2" 3)
      `shouldBe` Left (PersonNotFound uid)
    planCommand later history (Just aid) Nothing (RenameOrganization bid "B2" 3)
      `shouldSatisfy` isLeft
    planCommand later history Nothing Nothing (AddPerson (Person uid "New" "Role" Nothing))
      `shouldBe` Left AmbiguousOrganizations
  it "rejects stale versions without creating events" $
    planCommand later history (Just aid) Nothing (RenameOrganization aid "A2" 1)
      `shouldBe` Left (VersionConflict 1 2)
  it "plans a complete demo batch after unrelated events without changing their state" $ do
    additions <- either (fail . show) pure (planDemo later history)
    map storedSeq additions `shouldBe` [4 .. 55]
    old <- either (fail . show) pure (replayRegistry history)
    next <- either (fail . show) pure (replayRegistry (history <> additions))
    map (\oid -> Map.lookup oid (registryStates next)) [aid, bid]
      `shouldBe` map (\oid -> Map.lookup oid (registryStates old)) [aid, bid]
    planDemo later (history <> additions) `shouldBe` Left OrganizationAlreadyExists
  it "releases an acquired adapter if its initial sequence is invalid" $ do
    released <- newIORef False
    outcome <-
      try
        ( Runtime.openStore
            ( Persistence
                [StoredEvent 2 now Nothing (OrganizationCreated (Organization aid "A" now))]
                (const (pure ()))
                (writeIORef released True)
            )
        )
        :: IO (Either IOException Runtime.Store)
    either (const True) (const False) outcome `shouldBe` True
    readIORef released `shouldReturn` True
  it "does not publish state when its persistence port fails" $ do
    store <-
      Runtime.openStore
        (Persistence history (const (ioError (userError "disk unavailable"))) (pure ()))
    Runtime.runOrganizationCommand store aid Nothing (RenameOrganization aid "Failed" 2)
      `shouldReturn` Left StorageFailure
    Runtime.readAudit store `shouldReturn` history
    Runtime.closeStore store
  it
    "rethrows interruption without publishing unpersisted events or losing the runtime lock" $ do
    store <- Runtime.openStore (Persistence history (const (throwIO ThreadKilled)) (pure ()))
    outcome <-
      try
        (Runtime.runOrganizationCommand store aid Nothing (RenameOrganization aid "Interrupted" 2))
        :: IO (Either AsyncException (Either OrganizationError [StoredEvent]))
    outcome `shouldBe` Left ThreadKilled
    Runtime.readAudit store `shouldReturn` history
    Runtime.closeStore store

now, later :: UTCTime
now = read "2026-01-01 00:00:00 UTC"
later = read "2026-01-02 00:00:00 UTC"

aid, bid :: OrgId
aid = OrgId "a"
bid = OrgId "b"

uid :: UserId
uid = UserId "a-owner"

history :: [StoredEvent]
history =
  zipWith
    (\n event -> StoredEvent n now Nothing event)
    [1 ..]
    [ OrganizationScoped aid (OrganizationCreated (Organization aid "A" now))
    , OrganizationScoped aid (PersonAdded (Person uid "Owner" "Role" Nothing))
    , OrganizationScoped bid (OrganizationCreated (Organization bid "B" now))
    ]
