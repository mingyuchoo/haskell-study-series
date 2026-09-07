module MyOrg.Application
  ( Command (..)
  , executeCommand
  ) where

import Data.Time (UTCTime)
import MyOrg.Application.Command.Authority qualified as Authority
import MyOrg.Application.Command.Discovery qualified as Discovery
import MyOrg.Application.Command.Goals qualified as Goals
import MyOrg.Application.Command.Organization qualified as Organization
import MyOrg.Application.Command.People qualified as People
import MyOrg.Application.Command.Review qualified as Review
import MyOrg.Application.Command.Types
import MyOrg.Domain.Error
import MyOrg.Domain.Event.Types
import MyOrg.Domain.State

executeCommand
  :: UTCTime -> OrgState -> Command -> Either OrganizationError [OrganizationEvent]
executeCommand now st command = fmap pure $ case command of
  CreateOrganization oid name -> Organization.createOrganization now st oid name
  RenameOrganization oid name expectedVersion -> Organization.renameOrganization st oid name expectedVersion
  DeleteOrganization oid confirmName expectedVersion -> Organization.deleteOrganization st oid confirmName expectedVersion
  AddPerson p -> People.addPerson st p
  AddEmployee p profile -> People.addEmployee st p profile
  UpdatePerson p profile version -> People.updatePerson st p profile version
  DeactivatePerson uid successor version -> People.deactivatePerson st uid successor version
  CreateGoal g -> Goals.createGoal st g
  AssignOwner gid uid -> Goals.assignOwner st gid uid
  ActivateGoal gid -> Goals.activateGoal st gid
  ChangeStrategy gid note -> Goals.changeStrategy st gid note
  GrantGoalAuthority gid a -> Authority.grantGoalAuthority st gid a
  GrantAuthority a -> Authority.grantAuthority st a
  RevokeAuthority uid permission -> Authority.revokeAuthority st uid permission
  ReportResult gid value uid note -> Review.reportResult now st gid value uid note
  EvaluateGoal gid -> Review.evaluateGoalResult now st gid
  HoldReview rid gid learnings decisions note -> Review.holdReview now st rid gid learnings decisions note
  SaveDiscovery document version -> Discovery.saveDiscovery st document version
