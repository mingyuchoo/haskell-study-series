module MyOrg.Domain.Validation
  ( validateActive
  ) where

import Data.Map.Strict qualified as Map
import MyOrg.Domain.Authority
import MyOrg.Domain.Error
import MyOrg.Domain.Goal (ActiveGoal, validateGoal)
import MyOrg.Domain.Identity
import MyOrg.Domain.Queries
import MyOrg.Domain.State

-- | 현재 상태에서 목표를 활성화할 수 있는지 검증한다.
validateActive :: OrgState -> GoalId -> Either OrganizationError ActiveGoal
validateActive st gid = do
  g <- maybe (Left (GoalNotFound gid)) Right (Map.lookup gid (stateGoals st))
  let o = goalOwnership st gid
      a = o >>= ownerAuthority st . ownershipOwner
  case o of
    Just own -> requireActivePerson st (ownershipOwner own) >> validateGoal g o a
    _        -> validateGoal g o a
