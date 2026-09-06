-- | HTTP path and request parsing. All resource names stop at this boundary.
module MyOrg.Http.Route (readRoute, writeRoute, errorStatus) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Network.HTTP.Types hiding (Query)
import MyOrg.Domain.Identity
import MyOrg.Domain.Authority
import MyOrg.Domain.Error
import MyOrg.Application
import MyOrg.Serialization.JSON (parseWire, field, optionalField)
import MyOrg.Application.Query

readRoute :: [Text] -> Maybe Query
readRoute path = case path of
  ["api", "organizations"] -> Just ListOrganizations
  ["api", "organizations", oid] -> Just (OrganizationSummaryQuery (OrgId oid))
  ["api", "organizations", oid, resource] -> OrganizationQuery (SelectedOrganization (OrgId oid)) <$> resourceName resource
  ["api", resource] -> OrganizationQuery SoleOrganization <$> resourceName resource
  _ -> Nothing
 where
  resourceName = \case
    "dashboard" -> Just DashboardResource
    "organization" -> Just OrganizationResource
    "people" -> Just PeopleResource
    "goals" -> Just GoalsResource
    "compiler" -> Just CompilerResource
    "graph" -> Just GraphResource
    "events" -> Just EventsResource
    "reviews" -> Just ReviewsResource
    _ -> Nothing

writeRoute :: Method -> [Text] -> Maybe (Maybe OrgId, Value -> Parser (Maybe UserId, Command))
writeRoute method path = case (method, path) of
  ("POST", ["api", "demo"]) -> Just (Nothing, const (fail "데모 요청은 JSON 객체여야 합니다."))
  ("POST", ["api", "organizations"]) -> Just (Nothing, parseCommand path)
  ("PATCH", ["api", "organizations", oid]) -> Just (Just (OrgId oid), withObject "rename" $ \o ->
    (,) <$> optionalField o "actor" <*> (RenameOrganization (OrgId oid) <$> field o "name" <*> field o "expectedVersion"))
  ("DELETE", ["api", "organizations", oid]) -> Just (Just (OrgId oid), withObject "delete" $ \o ->
    (,) <$> optionalField o "actor" <*> (DeleteOrganization (OrgId oid) <$> field o "confirmName" <*> field o "expectedVersion"))
  ("POST", "api":"organizations":oid:rest) | knownCommand ("api":rest) -> Just (Just (OrgId oid), parseCommand ("api":rest))
  ("POST", _) | knownCommand path -> Just (Nothing, parseCommand path)
  _ -> Nothing

knownCommand :: [Text] -> Bool
knownCommand path = case path of
  ["api", name] -> name `elem` ["people", "goals", "evaluations", "reviews"]
  ["api", "goals", _, action] -> action `elem` ["owner", "authority", "activate", "results", "strategy"]
  ["api", "people", _, "authority"] -> True
  _ -> False

parseCommand :: [Text] -> Value -> Parser (Maybe UserId, Command)
parseCommand path = withObject "command" $ \o -> do
  actor <- optionalField o "actor"
  command <- case path of
    ["api","organizations"] -> CreateOrganization <$> field o "id" <*> field o "name"
    ["api","people"] -> AddPerson <$> parseWire (Object o)
    ["api","goals"] -> CreateGoal <$> parseWire (Object o)
    ["api","goals",gid,"owner"] -> AssignOwner (GoalId gid) <$> field o "owner"
    ["api","people",uid,"authority"] -> do
      a <- parseWire (Object o)
      if authorityOwner a /= UserId uid then fail "권한 소유자가 경로와 일치하지 않습니다." else pure (GrantAuthority a)
    ["api","goals",gid,"authority"] -> GrantGoalAuthority (GoalId gid) <$> parseWire (Object o)
    ["api","goals",gid,"activate"] -> pure (ActivateGoal (GoalId gid))
    ["api","goals",gid,"results"] -> ReportResult (GoalId gid) <$> field o "value" <*> field o "reportedBy" <*> field o "note"
    ["api","evaluations"] -> EvaluateGoal <$> field o "goal"
    ["api","reviews"] -> HoldReview <$> field o "id" <*> field o "goal" <*> optionalField o "learnings" .!= [] <*> optionalField o "decisions" .!= [] <*> field o "note"
    ["api","goals",gid,"strategy"] -> ChangeStrategy (GoalId gid) <$> field o "note"
    _ -> fail "경로를 찾을 수 없습니다."
  pure (actor, command)

errorStatus :: OrganizationError -> Status
errorStatus = \case
  AmbiguousOrganizations -> status409
  OrganizationNotFound _ -> status404
  VersionConflict _ _ -> status409
  StorageFailure -> status500
  GoalNotFound _ -> status404
  PersonNotFound _ -> status404
  DuplicateId _ -> status409
  OrganizationAlreadyExists -> status409
  GoalAlreadyActive _ -> status409
  GoalNotActive _ -> status409
  _ -> status400
