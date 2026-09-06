module MyOrg.Domain.Validation (validateActive) where

import qualified Data.Map.Strict as Map
import MyOrg.Domain.State
import MyOrg.Domain.Queries
import MyOrg.Domain.Goal (ActiveGoal, validateGoal)
import MyOrg.Domain.Identity
import MyOrg.Domain.Authority
import MyOrg.Domain.Error

-- | 현재 상태에서 목표를 활성화할 수 있는지 검증한다.
validateActive :: OrgState -> GoalId -> Either OrganizationError ActiveGoal
validateActive st gid = do
  g <- maybe (Left (GoalNotFound gid)) Right (Map.lookup gid (stateGoals st))
  let o = goalOwnership st gid
      a = o >>= ownerAuthority st . ownershipOwner
  case o of
    Just own | not (Map.member (ownershipOwner own) (statePeople st)) -> Left (PersonNotFound (ownershipOwner own))
    _ -> validateGoal g o a
