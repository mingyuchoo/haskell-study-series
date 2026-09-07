module MyOrg.Domain.Event.Types
  ( OrganizationEvent (..)
  , StoredEvent (..)
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import MyOrg.Domain.Agent
import MyOrg.Domain.Authority
import MyOrg.Domain.Discovery
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Identity
import MyOrg.Domain.Organization
import MyOrg.Domain.Result
import MyOrg.Domain.Review.Types

data OrganizationEvent = OrganizationCreated Organization
                       | OrganizationScoped OrgId OrganizationEvent
                       | OrganizationRenamed OrgId Text
                       | OrganizationDeleted OrgId
                       | DemoSeeded OrgId
                       | PersonAdded Person
                       | EmployeeAdded Person EmployeeProfile
                       | PersonUpdated Person EmployeeProfile
                       | PersonDeactivated UserId (Maybe UserId)
                       | GoalCreated Goal
                       | OwnerAssigned GoalId UserId
                       | AuthorityGranted UserId Authority
                       | AuthorityRevoked UserId Permission
                       | GoalActivated GoalId
                       | ResultReported GoalId Result
                       | GoalEvaluated GoalId Evaluation
                       | ReviewHeld Review
                       | StrategyChanged GoalId Text
                       | DiscoverySaved Discovery
                       | AgentRolesSaved [AgentRole]
  deriving stock (Show, Eq, Generic)

-- | 저장소에 기록된 이벤트. 순번과 시각, 행위자를 함께 남긴다.
data StoredEvent = StoredEvent
  { storedSeq   :: Int
  , storedAt    :: UTCTime
  , storedActor :: Maybe UserId
  , storedEvent :: OrganizationEvent
  }
  deriving stock (Show, Eq, Generic)
