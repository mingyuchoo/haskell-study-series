module MyOrg.Application.Command.Authority
  ( grantGoalAuthority
  , grantAuthority
  , revokeAuthority
  ) where

import Control.Monad (unless, when)
import MyOrg.Domain.Authority
import MyOrg.Domain.Error
import MyOrg.Domain.Event.Types
import MyOrg.Domain.Identity
import MyOrg.Domain.Queries
import MyOrg.Domain.State

import MyOrg.Application.Command.Validation

grantGoalAuthority
  :: OrgState -> GoalId -> Authority -> Either OrganizationError OrganizationEvent
grantGoalAuthority st gid a = do
  _ <- requireGoal st gid
  owner <- maybe (Left (NoOwner gid)) Right (goalOwner st gid)
  unless (owner == authorityOwner a) (Left (OwnerMismatch gid owner (authorityOwner a)))
  grantAuthority st a

grantAuthority :: OrgState -> Authority -> Either OrganizationError OrganizationEvent
grantAuthority st a = do
  validateAuthority st a
  pure (AuthorityGranted (authorityOwner a) a)

validateAuthority :: OrgState -> Authority -> Either OrganizationError ()
validateAuthority st a = do
  requireActivePerson st (authorityOwner a)
  when (authorityBudgetLimit a < 0) (Left (InvalidInput "예산은 음수일 수 없습니다."))

revokeAuthority
  :: OrgState -> UserId -> Permission -> Either OrganizationError OrganizationEvent
revokeAuthority st uid permission = requireActivePerson st uid >> pure (AuthorityRevoked uid permission)
