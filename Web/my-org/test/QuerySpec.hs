module QuerySpec (spec) where

import Data.Time (UTCTime, addUTCTime)
import MyOrg.Application.Query
import MyOrg.Application.ReadModel
import MyOrg.Demo (demoEvents, demoOrganizationId)
import MyOrg.Domain.Event
import MyOrg.Http.Route (readRoute)
import MyOrg.Registry
import MyOrg.Types
import Test.Hspec

spec :: Spec
spec = describe "typed queries" $ do
  it "preserves empty legacy organization and dashboard results" $ do
    registry <- either (fail . show) pure (replayRegistry [])
    executeQuery now registry (OrganizationQuery SoleOrganization OrganizationResource) `shouldBe` Right (OrganizationResult Nothing)
    case executeQuery now registry (OrganizationQuery SoleOrganization DashboardResource) of
      Right (DashboardResult dashboard) -> do
        dashboardOrganization dashboard `shouldBe` Nothing
        dashboardVersion dashboard `shouldBe` 0
        dashboardGoals dashboard `shouldBe` []
        dashboardEvents dashboard `shouldBe` []
      value -> expectationFailure (show value)
    executeQuery now registry (OrganizationSummaryQuery (OrgId "missing")) `shouldBe` Left (OrganizationNotFound (OrgId "missing"))
  it "projects deterministic evaluations, counts and timeline ordering using the supplied time" $ do
    events <- either (fail . show) pure (demoEvents now)
    registry <- either (fail . show) pure (replayRegistry events)
    let query = OrganizationQuery (SelectedOrganization demoOrganizationId) DashboardResource
    case executeQuery (addUTCTime 86400 now) registry query of
      Right (DashboardResult dashboard) -> do
        map (evaluationEvaluatedAt . viewEvaluation) (dashboardGoals dashboard) `shouldBe` replicate 7 (addUTCTime 86400 now)
        dashboardEvents dashboard `shouldBe` reverse events
        length (dashboardPeople dashboard) `shouldBe` 6
        dashboardDemo dashboard `shouldBe` True
        executeQuery now registry (OrganizationQuery SoleOrganization EventsResource) `shouldBe` Right (EventsResult events)
      value -> expectationFailure (show value)
  it "requires explicit scope with multiple organizations and exposes only the recreated epoch" $ do
    let a = OrgId "a"
        b = OrgId "b"
        event oid payload = OrganizationScoped oid payload
        events = zipWith (\n payload -> StoredEvent n now Nothing payload) [1..]
          [ event a (OrganizationCreated (Organization a "A" now))
          , event b (OrganizationCreated (Organization b "B" now))
          , event a (OrganizationDeleted a)
          , event a (OrganizationCreated (Organization a "New A" now)) ]
    registry <- either (fail . show) pure (replayRegistry events)
    executeQuery now registry (OrganizationQuery SoleOrganization EventsResource) `shouldBe` Left AmbiguousOrganizations
    executeQuery now registry (OrganizationQuery (SelectedOrganization a) EventsResource) `shouldBe`
      Right (EventsResult [StoredEvent 4 now Nothing (OrganizationCreated (Organization a "New A" now))])
  it "maps only known HTTP resources to typed queries" $ do
    readRoute ["api", "organizations", "a", "goals"] `shouldBe` Just (OrganizationQuery (SelectedOrganization (OrgId "a")) GoalsResource)
    readRoute ["api", "dashboard"] `shouldBe` Just (OrganizationQuery SoleOrganization DashboardResource)
    readRoute ["api", "organizations", "a", "unknown"] `shouldBe` Nothing
    readRoute ["api", "organizations", "a", "goals", "extra"] `shouldBe` Nothing

now :: UTCTime
now = read "2026-01-01 00:00:00 UTC"
