-- Fixed wire fixtures deliberately live outside the test's generation path.
module ContractSpec
  ( spec
  , fixtureTime
  , normalizeDashboard
  , withFixtureServer
  ) where

import Control.Exception (bracket)
import Data.Aeson (Value (..), eitherDecode, encode)
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime)
import MyOrg.Demo (demoEvents, demoOrganizationId)
import MyOrg.Domain.Event
import MyOrg.Registry
import MyOrg.Serialization.JSON (Wire, eitherDecodeWire, toWire)
import MyOrg.Server (application)
import MyOrg.Store (closeStore, openFileStore)
import MyOrg.Types (OrgId (..), Permission (..), UserId (..))
import Network.Wai.Handler.Warp (testWithApplication)
import SmokeSupport (Client, field, get, items, withClient)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

fixtureTime :: UTCTime
fixtureTime = read "2000-01-01 00:00:00 UTC"

spec :: Spec
spec = describe "persisted event and Elm HTTP contracts" $ do
  it "decodes historical legacy and scoped logs and preserves their exact JSON shapes" $ do
    baseline <- either (fail . show) pure (demoEvents fixtureTime)
    let scoped =
          map
            (\event -> event {storedEvent = OrganizationScoped demoOrganizationId (storedEvent event)})
            baseline
    mapM_
      ( \(name, expected) -> do
          bytes <- BL.readFile (fixturePath name)
          decoded <- decodeFixture bytes
          decoded `shouldBe` expected
          raw <- decodeFixture bytes
          toWire (decoded :: [StoredEvent]) `shouldBe` (raw :: Value)
          registry <- either (fail . show) pure (replayRegistry decoded)
          state <- either (fail . show) pure (organizationState registry demoOrganizationId)
          state `shouldBe` replay baseline
          Map.size (statePeople state) `shouldBe` 6
          Map.size (stateGoals state) `shouldBe` 7
          Set.size (stateActive state) `shouldBe` 5
          length (stateReviews state) `shouldBe` 2
      )
      [("legacy-events.json", baseline), ("scoped-events.json", scoped)]
  it "preserves lifecycle and revoked-permission event tags absent from the demo" $ do
    let raw =
          "[{\"seq\":1,\"at\":\"2000-01-01T00:00:00Z\",\"event\":{\"tag\":\"OrganizationRenamed\",\"contents\":[\"org\",\"Renamed\"]}},{\"seq\":2,\"at\":\"2000-01-01T00:00:00Z\",\"actor\":\"owner\",\"event\":{\"tag\":\"AuthorityRevoked\",\"contents\":[\"owner\",\"Pricing\"]}},{\"seq\":3,\"at\":\"2000-01-01T00:00:00Z\",\"event\":{\"tag\":\"OrganizationDeleted\",\"contents\":\"org\"}}]"
        expected =
          [ StoredEvent 1 fixtureTime Nothing (OrganizationRenamed (OrgId "org") "Renamed")
          , StoredEvent
              2
              fixtureTime
              (Just (UserId "owner"))
              (AuthorityRevoked (UserId "owner") Pricing)
          , StoredEvent 3 fixtureTime Nothing (OrganizationDeleted (OrgId "org"))
          ]
    decodeFixture raw `shouldReturn` expected
    decodedValue <- decodeFixture raw
    toWire expected `shouldBe` (decodedValue :: Value)
  it "serves the fixed dashboard contract from either historical storage format" $ do
    expected <- BL.readFile (fixturePath "dashboard.json") >>= decodeFixture
    mapM_
      ( \name -> withFixtureServer name $ \client -> do
          dashboard <- get client "organizations/demo-northstar-v2/dashboard"
          -- Only read-time evaluation timestamps vary. Require valid timestamps
          -- before replacing them, retaining every key, null, list and other value.
          mapM_
            ( \entry -> case field (field entry "evaluation") "evaluatedAt" of
                String value ->
                  (eitherDecode (encode value) :: Either String UTCTime)
                    `shouldSatisfy` either (const False) (const True)
                value -> expectationFailure ("Invalid evaluation timestamp: " <> show value)
            )
            (items (field dashboard "goals"))
          normalizeDashboard dashboard `shouldBe` expected
          legacy <- get client "dashboard"
          normalizeDashboard legacy `shouldBe` expected
      )
      ["legacy-events.json", "scoped-events.json"]

fixturePath :: FilePath -> FilePath
fixturePath name = "test/fixtures" </> name

decodeFixture :: (Wire a) => BL.ByteString -> IO a
decodeFixture = either fail pure . eitherDecodeWire

withFixtureServer :: FilePath -> (Client -> IO a) -> IO a
withFixtureServer name action = withSystemTempDirectory "my-org-contract-" $ \directory -> do
  let path = directory </> "events.json"
  BL.readFile (fixturePath name) >>= BL.writeFile path
  bracket (openFileStore path) closeStore $ \store ->
    testWithApplication (pure (application store)) $ \port -> withClient port action

normalizeDashboard :: Value -> Value
normalizeDashboard (Object dashboard) = Object (adjust normalizeGoals "goals" dashboard)
  where
    normalizeGoals (Array goals) = Array (fmap normalizeGoal goals)
    normalizeGoals value         = value
    normalizeGoal (Object goal) = Object (adjust normalizeEvaluation "evaluation" goal)
    normalizeGoal value         = value
    normalizeEvaluation (Object evaluation) = Object (adjust (const (String "<read-time>")) "evaluatedAt" evaluation)
    normalizeEvaluation value = value
normalizeDashboard value = value

-- Keep missing fields missing so normalization cannot hide contract drift.
adjust :: (Value -> Value) -> Data.Aeson.Key.Key -> KM.KeyMap Value -> KM.KeyMap Value
adjust f target = KM.mapWithKey (\key value -> if key == target then f value else value)
