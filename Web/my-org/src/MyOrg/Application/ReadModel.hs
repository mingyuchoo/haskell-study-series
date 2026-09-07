-- | Typed query results independent of HTTP paths and JSON representation.
module MyOrg.Application.ReadModel
  ( PersonView (..)
  , OrganizationSummary (..)
  , GoalView (..)
  , Dashboard (..)
  , QueryResult (..)
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import MyOrg.Domain.Analysis (Analysis)
import MyOrg.Domain.Authority
import MyOrg.Domain.Compiler (CompileReport)
import MyOrg.Domain.Discovery
import MyOrg.Domain.Event.Types
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Graph (ResponsibilityGraph)
import MyOrg.Domain.Identity
import MyOrg.Domain.Organization
import MyOrg.Domain.Result
import MyOrg.Domain.Review (ReviewWarning)
import MyOrg.Domain.Review.Types

data PersonView = PersonView
  { viewPerson       :: Person
  , viewProfile      :: EmployeeProfile
  , viewPersonActive :: Bool
  }
  deriving (Show, Eq)

data OrganizationSummary = OrganizationSummary
  { summaryOrganization :: Maybe Organization
  , summaryVersion      :: Int
  , summaryDemo         :: Bool
  , summaryPeopleCount  :: Int
  , summaryGoalCount    :: Int
  }
  deriving (Show, Eq)

data GoalView = GoalView
  { viewGoal       :: Goal
  , viewOwner      :: Maybe UserId
  , viewActive     :: Bool
  , viewEvaluation :: Evaluation
  , viewAnalysis   :: Maybe Analysis
  , viewResults    :: [Result]
  , viewStrategies :: [(UTCTime, Text)]
  }
  deriving (Show, Eq)

data Dashboard = Dashboard
  { dashboardVersion        :: Int
  , dashboardDemo           :: Bool
  , dashboardOrganization   :: Maybe Organization
  , dashboardPeople         :: [PersonView]
  , dashboardGoals          :: [GoalView]
  , dashboardAuthorities    :: [Authority]
  , dashboardCompiler       :: CompileReport
  , dashboardGraph          :: ResponsibilityGraph
  , dashboardDecisionShare  :: Map UserId Double
  , dashboardReviews        :: [Review]
  , dashboardReviewWarnings :: [(ReviewId, [ReviewWarning])]
  , dashboardEvents         :: [StoredEvent]
  }
  deriving (Show, Eq)

data QueryResult = OrganizationsResult [OrganizationSummary]
                 | SummaryResult OrganizationSummary
                 | DashboardResult Dashboard
                 | OrganizationResult (Maybe Organization)
                 | PeopleResult [PersonView]
                 | PersonResult PersonView Int [GoalId]
                 | GoalsResult [GoalView]
                 | CompilerResult CompileReport
                 | GraphResult ResponsibilityGraph
                 | EventsResult [StoredEvent]
                 | ReviewsResult [Review]
                 | DiscoveryResult Int Discovery
  deriving (Show, Eq)
