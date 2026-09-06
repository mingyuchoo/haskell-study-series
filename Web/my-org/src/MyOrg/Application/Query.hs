-- | Pure organization selection and projection. Transport chooses the query.
module MyOrg.Application.Query (Query(..), Selection(..), Resource(..), executeQuery) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (UTCTime)
import MyOrg.Domain.Identity
import MyOrg.Domain.Organization
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Review.Types
import MyOrg.Domain.Error
import MyOrg.Registry
import MyOrg.Demo (isDemoEpoch)
import MyOrg.Domain.State
import MyOrg.Domain.Queries
import MyOrg.Domain.Reducer
import MyOrg.Domain.Evaluation (evaluateGoal)
import MyOrg.Domain.Compiler (compileOrganization)
import MyOrg.Domain.Graph (buildGraph, decisionShare)
import MyOrg.Domain.Analysis (analyzeGoal)
import MyOrg.Domain.Review (checkReview)
import MyOrg.Application.ReadModel

data Selection = SoleOrganization | SelectedOrganization OrgId deriving (Show, Eq)
data Resource = DashboardResource | OrganizationResource | PeopleResource | GoalsResource
  | CompilerResource | GraphResource | EventsResource | ReviewsResource deriving (Show, Eq)
data Query = ListOrganizations | OrganizationSummaryQuery OrgId | OrganizationQuery Selection Resource deriving (Show, Eq)

executeQuery :: UTCTime -> Registry -> Query -> Either OrganizationError QueryResult
executeQuery now registry query = case query of
  ListOrganizations -> pure (OrganizationsResult (map summary (activeOrganizations registry)))
  OrganizationSummaryQuery oid -> SummaryResult . summary <$> organizationState registry oid
  OrganizationQuery selection resource -> do
    st <- case selection of
      SelectedOrganization oid -> organizationState registry oid
      SoleOrganization -> resolveSingleOrganization registry >>= maybe (pure emptyState {stateLastSeq = registryLastSeq registry}) (organizationState registry)
    pure (project st resource)
 where
  history st = maybe [] (\org -> Map.findWithDefault [] (organizationId org) (registryEvents registry)) (stateOrganization st)
  summary st = OrganizationSummary (stateOrganization st) (stateLastSeq st) (isDemoEpoch (history st)) (Map.size (statePeople st)) (Map.size (stateGoals st))
  goals st = [GoalView g (goalOwner st (goalId g)) (Set.member (goalId g) (stateActive st))
    (evaluateGoal now g (resultsOf st (goalId g)))
    (either (const Nothing) Just (analyzeGoal st (goalId g)))
    (resultsOf st (goalId g)) (Map.findWithDefault [] (goalId g) (stateStrategies st))
    | g <- Map.elems (stateGoals st)]
  project st resource = case resource of
    DashboardResource -> DashboardResult Dashboard
      { dashboardVersion = stateLastSeq st, dashboardDemo = isDemoEpoch (history st)
      , dashboardOrganization = stateOrganization st, dashboardPeople = Map.elems (statePeople st)
      , dashboardGoals = goals st, dashboardAuthorities = Map.elems (stateAuthorities st)
      , dashboardCompiler = compileOrganization now st, dashboardGraph = buildGraph st
      , dashboardDecisionShare = decisionShare st, dashboardReviews = stateReviews st
      , dashboardReviewWarnings = [(reviewId review, checkReview review) | review <- stateReviews st]
      , dashboardEvents = reverse (currentEpoch (history st))
      }
    OrganizationResource -> OrganizationResult (stateOrganization st)
    PeopleResource -> PeopleResult (Map.elems (statePeople st))
    GoalsResource -> GoalsResult (goals st)
    CompilerResource -> CompilerResult (compileOrganization now st)
    GraphResource -> GraphResult (buildGraph st)
    EventsResource -> EventsResult (currentEpoch (history st))
    ReviewsResource -> ReviewsResult (stateReviews st)
