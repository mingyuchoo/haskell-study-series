-- | Typed query results independent of HTTP paths and JSON representation.
module MyOrg.Application.ReadModel
  ( OrganizationSummary(..), GoalView(..), Dashboard(..), QueryResult(..) ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import MyOrg.Domain.Identity
import MyOrg.Domain.Organization
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Authority
import MyOrg.Domain.Result
import MyOrg.Domain.Review.Types
import MyOrg.Domain.Analysis (Analysis)
import MyOrg.Domain.Compiler (CompileReport)
import MyOrg.Domain.Event.Types
import MyOrg.Domain.Graph (ResponsibilityGraph)
import MyOrg.Domain.Review (ReviewWarning)

data OrganizationSummary = OrganizationSummary
  { summaryOrganization :: Maybe Organization
  , summaryVersion :: Int
  , summaryDemo :: Bool
  , summaryPeopleCount :: Int
  , summaryGoalCount :: Int
  } deriving (Show, Eq)

data GoalView = GoalView
  { viewGoal :: Goal
  , viewOwner :: Maybe UserId
  , viewActive :: Bool
  , viewEvaluation :: Evaluation
  , viewAnalysis :: Maybe Analysis
  , viewResults :: [Result]
  , viewStrategies :: [(UTCTime, Text)]
  } deriving (Show, Eq)

data Dashboard = Dashboard
  { dashboardVersion :: Int
  , dashboardDemo :: Bool
  , dashboardOrganization :: Maybe Organization
  , dashboardPeople :: [Person]
  , dashboardGoals :: [GoalView]
  , dashboardAuthorities :: [Authority]
  , dashboardCompiler :: CompileReport
  , dashboardGraph :: ResponsibilityGraph
  , dashboardDecisionShare :: Map UserId Double
  , dashboardReviews :: [Review]
  , dashboardReviewWarnings :: [(ReviewId, [ReviewWarning])]
  , dashboardEvents :: [StoredEvent]
  } deriving (Show, Eq)

data QueryResult
  = OrganizationsResult [OrganizationSummary]
  | SummaryResult OrganizationSummary
  | DashboardResult Dashboard
  | OrganizationResult (Maybe Organization)
  | PeopleResult [Person]
  | GoalsResult [GoalView]
  | CompilerResult CompileReport
  | GraphResult ResponsibilityGraph
  | EventsResult [StoredEvent]
  | ReviewsResult [Review]
  deriving (Show, Eq)
