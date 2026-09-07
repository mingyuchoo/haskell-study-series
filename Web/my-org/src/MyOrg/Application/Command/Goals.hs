module MyOrg.Application.Command.Goals
  ( createGoal
  , assignOwner
  , activateGoal
  , changeStrategy
  ) where

import Control.Monad (unless, when)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import MyOrg.Domain.Error
import MyOrg.Domain.Event.Types
import MyOrg.Domain.Goal (validateDraft)
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Identity
import MyOrg.Domain.Organization
import MyOrg.Domain.Queries
import MyOrg.Domain.State
import MyOrg.Domain.Validation

import MyOrg.Application.Command.Validation

createGoal :: OrgState -> Goal -> Either OrganizationError OrganizationEvent
createGoal st g = do
  org <- maybe (Left NoOrganization) Right (stateOrganization st)
  unless
    (goalOrganization g == organizationId org)
    (Left (InvalidInput "목표의 조직이 일치하지 않습니다."))
  identifier (unGoalId (goalId g))
  duplicate (Map.member (goalId g) (stateGoals st)) (unGoalId (goalId g))
  validateDraft g
  mapM_
    (\p -> unless (Map.member p (stateGoals st)) (Left (ParentGoalNotFound (goalId g) p)))
    (goalParent g)
  pure (GoalCreated g)

assignOwner :: OrgState -> GoalId -> UserId -> Either OrganizationError OrganizationEvent
assignOwner st gid uid = requireGoal st gid >> requireActivePerson st uid >> pure (OwnerAssigned gid uid)

activateGoal :: OrgState -> GoalId -> Either OrganizationError OrganizationEvent
activateGoal st gid = do
  when (Set.member gid (stateActive st)) (Left (GoalAlreadyActive gid))
  _ <- validateActive st gid
  pure (GoalActivated gid)

changeStrategy :: OrgState -> GoalId -> Text -> Either OrganizationError OrganizationEvent
changeStrategy st gid note = requireGoal st gid >> nonempty note >> pure (StrategyChanged gid note)
