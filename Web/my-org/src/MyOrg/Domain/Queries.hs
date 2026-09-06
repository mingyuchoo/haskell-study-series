module MyOrg.Domain.Queries
  ( requireActivePerson
  , activeGoals
  , draftGoals
  , goalOwnership
  , goalOwner
  , ownerAuthority
  , resultsOf
  ) where

import Control.Monad (unless, when)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import MyOrg.Domain.Authority
import MyOrg.Domain.Error
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Identity
import MyOrg.Domain.Result
import MyOrg.Domain.State

-- | 활성화된 목표 목록.
activeGoals :: OrgState -> [Goal]
activeGoals st =
  [g | g <- Map.elems (stateGoals st), Set.member (goalId g) (stateActive st)]

draftGoals :: OrgState -> [Goal]
draftGoals st =
  [g | g <- Map.elems (stateGoals st), not (Set.member (goalId g) (stateActive st))]

goalOwnership :: OrgState -> GoalId -> Maybe Ownership
goalOwnership st gid = Map.lookup gid (stateOwnership st)

goalOwner :: OrgState -> GoalId -> Maybe UserId
goalOwner st gid = ownershipOwner <$> goalOwnership st gid

ownerAuthority :: OrgState -> UserId -> Maybe Authority
ownerAuthority st uid = Map.lookup uid (stateAuthorities st)

resultsOf :: OrgState -> GoalId -> [Result]
resultsOf st gid = Map.findWithDefault [] gid (stateResults st)

requireActivePerson :: OrgState -> UserId -> Either OrganizationError ()
requireActivePerson st uid = do
  unless (Map.member uid (statePeople st)) (Left (PersonNotFound uid))
  when
    (Set.member uid (stateInactivePeople st))
    (Left (InvalidInput "비활성 구성원은 새 배정이나 기록에 사용할 수 없습니다."))
