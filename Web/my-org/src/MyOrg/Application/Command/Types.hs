module MyOrg.Application.Command.Types
  ( Command (..)
  ) where

import Data.Text (Text)
import MyOrg.Domain.Authority
import MyOrg.Domain.Discovery
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Identity
import MyOrg.Domain.Organization
import MyOrg.Domain.Review.Types

data Command = CreateOrganization OrgId Text
             | RenameOrganization OrgId Text Int
             | DeleteOrganization OrgId Text Int
             | AddPerson Person
             | AddEmployee Person EmployeeProfile
             | UpdatePerson Person EmployeeProfile Int
             | DeactivatePerson UserId (Maybe UserId) Int
             | CreateGoal Goal
             | AssignOwner GoalId UserId
             | GrantGoalAuthority GoalId Authority
             | GrantAuthority Authority
             | RevokeAuthority UserId Permission
             | ActivateGoal GoalId
             | ReportResult GoalId Double UserId Text
             | EvaluateGoal GoalId
             | HoldReview ReviewId GoalId [Learning] [Decision] Text
             | ChangeStrategy GoalId Text
             | SaveDiscovery Discovery Int
  deriving (Show, Eq)
