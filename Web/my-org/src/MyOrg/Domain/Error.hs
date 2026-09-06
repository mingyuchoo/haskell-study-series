-- | Pure Error domain concepts.
module MyOrg.Domain.Error (OrganizationError(..)) where

import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import MyOrg.Domain.Identity
import MyOrg.Domain.Authority (Permission)

-- | 조직 상태가 규칙을 어길 때 발생하는 오류.
data OrganizationError
  = NoOrganization
  | AmbiguousOrganizations
  | OrganizationNotFound OrgId
  | VersionConflict Int Int
  | OrganizationAlreadyExists
  | GoalNotFound GoalId
  | PersonNotFound UserId
  | NoOwner GoalId
  | OwnerMismatch GoalId UserId UserId
  -- ^ 목표, 책임자, 권한 소유자가 서로 다름
  | NoAuthority UserId
  | MissingPermissions GoalId UserId (Set Permission)
  | InsufficientBudget GoalId Money Money
  -- ^ 목표, 필요 예산, 부여된 예산
  | InvalidTarget GoalId
  | DeadlineBeforeStart GoalId
  | GoalAlreadyActive GoalId
  | GoalNotActive GoalId
  | ParentGoalNotFound GoalId GoalId
  | StorageFailure
  | InvalidInput Text
  | DuplicateId Text
  deriving stock (Show, Eq, Generic)
