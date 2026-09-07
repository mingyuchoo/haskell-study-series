module MyOrg.Application.Command.Organization
  ( createOrganization
  , renameOrganization
  , deleteOrganization
  ) where

import Control.Monad (unless, when)
import Data.Text (Text)
import Data.Time (UTCTime)
import MyOrg.Domain.Error
import MyOrg.Domain.Event.Types
import MyOrg.Domain.Identity
import MyOrg.Domain.Organization
import MyOrg.Domain.State

import MyOrg.Application.Command.Validation

createOrganization
  :: UTCTime -> OrgState -> OrgId -> Text -> Either OrganizationError OrganizationEvent
createOrganization now st oid name = do
  when (stateOrganization st /= Nothing) (Left OrganizationAlreadyExists)
  identifier (unOrgId oid)
  nonempty name
  pure (OrganizationCreated (Organization oid name now))

renameOrganization
  :: OrgState -> OrgId -> Text -> Int -> Either OrganizationError OrganizationEvent
renameOrganization st oid name expectedVersion = do
  org <- maybe (Left (OrganizationNotFound oid)) Right (stateOrganization st)
  unless (organizationId org == oid) (Left (OrganizationNotFound oid))
  unless
    (expectedVersion == stateLastSeq st)
    (Left (VersionConflict expectedVersion (stateLastSeq st)))
  nonempty name
  pure (OrganizationRenamed oid name)

deleteOrganization
  :: OrgState -> OrgId -> Text -> Int -> Either OrganizationError OrganizationEvent
deleteOrganization st oid confirmName expectedVersion = do
  org <- maybe (Left (OrganizationNotFound oid)) Right (stateOrganization st)
  unless (organizationId org == oid) (Left (OrganizationNotFound oid))
  unless
    (expectedVersion == stateLastSeq st)
    (Left (VersionConflict expectedVersion (stateLastSeq st)))
  unless
    (confirmName == organizationName org)
    (Left (InvalidInput "확인 이름이 현재 조직명과 정확히 일치해야 합니다."))
  pure (OrganizationDeleted oid)
