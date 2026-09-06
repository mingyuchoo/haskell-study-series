-- | Explicit, version-compatible JSON at HTTP and persistence boundaries.
-- Domain types have no Aeson instances. This module owns both the Wire class
-- and its instances, and the WireValue wrapper owns the only Aeson instances.
module MyOrg.Serialization.JSON
  ( Wire
  , WireValue (..)
  , toWire
  , parseWire
  , encodeWire
  , eitherDecodeWire
  , field
  , optionalField
  , (.=)
  ) where

import Data.Aeson (Key, Object, Value (..), object, withArray, withObject, withText)
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Pair, Parser)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable qualified as F
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import MyOrg.Domain.Analysis
import MyOrg.Domain.Authority
import MyOrg.Domain.Compiler
import MyOrg.Domain.Error
import MyOrg.Domain.Event.Types
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Graph
import MyOrg.Domain.Identity
import MyOrg.Domain.Organization
import MyOrg.Domain.Result
import MyOrg.Domain.Review
import MyOrg.Domain.Review.Types
import MyOrg.Presentation.Diagnostic (renderDiagnosticMessage)

class Wire a where
  toWire :: a -> Value
  parseWire :: Value -> Parser a

newtype WireValue a = WireValue { unWireValue :: a }

instance (Wire a) => A.ToJSON (WireValue a) where
  toJSON = toWire . unWireValue

instance (Wire a) => A.FromJSON (WireValue a) where
  parseJSON value = WireValue <$> parseWire value

encodeWire :: (Wire a) => a -> BL.ByteString
encodeWire = A.encode . WireValue

eitherDecodeWire :: (Wire a) => BL.ByteString -> Either String a
eitherDecodeWire bytes = unWireValue <$> A.eitherDecode bytes

infixr 8 .=
(.=) :: (Wire a) => Key -> a -> Pair
key .= value = (key, toWire value)

field :: (Wire a) => Object -> Key -> Parser a
field obj key = (obj A..: key) >>= parseWire

optionalField :: (Wire a) => Object -> Key -> Parser (Maybe a)
optionalField obj key = case KM.lookup key obj of
  Nothing    -> pure Nothing
  Just Null  -> pure Nothing
  Just value -> Just <$> parseWire value

record :: [(Key, Maybe Value)] -> Value
record = object . catMaybes . map (\(key, value) -> fmap (key,) value)

-- All constructor tags and record keys below are deliberately explicit. A
-- Haskell constructor/field rename must not silently migrate stored events.

instance Wire Value where
  toWire = A.toJSON
  parseWire = A.parseJSON

instance Wire Text where
  toWire = A.toJSON
  parseWire = A.parseJSON

instance Wire Bool where
  toWire = A.toJSON
  parseWire = A.parseJSON

instance Wire Int where
  toWire = A.toJSON
  parseWire = A.parseJSON

instance Wire Integer where
  toWire = A.toJSON
  parseWire = A.parseJSON

instance Wire Double where
  toWire = A.toJSON
  parseWire = A.parseJSON

instance Wire UTCTime where
  toWire = A.toJSON
  parseWire = A.parseJSON

instance (Wire a) => Wire [a] where
  toWire values = A.toJSON (map toWire values)
  parseWire = withArray "list" (mapM parseWire . F.toList)

instance (Wire a) => Wire (Maybe a) where
  toWire = maybe Null toWire
  parseWire Null  = pure Nothing
  parseWire value = Just <$> parseWire value

instance (Ord a, Wire a) => Wire (Set a) where
  toWire = toWire . Set.toList
  parseWire value = Set.fromList <$> parseWire value

instance (Wire a, Wire b) => Wire (a, b) where
  toWire (a, b) = toWire [toWire a, toWire b]
  parseWire = withArray "pair" $ \values -> case F.toList values of
    [a, b] -> (,) <$> parseWire a <*> parseWire b
    _      -> fail "Expected a two-element tuple"

instance (Wire a) => Wire (Map UserId a) where
  toWire values =
    object [(Key.fromText (unUserId key), toWire value) | (key, value) <- Map.toList values]
  parseWire = withObject "user map" $ \values ->
    Map.fromList
      <$> mapM (\(key, value) -> (UserId (Key.toText key),) <$> parseWire value) (KM.toList values)

instance Wire OrgId where
  toWire = toWire . unOrgId
  parseWire value = OrgId <$> parseWire value

instance Wire UserId where
  toWire = toWire . unUserId
  parseWire value = UserId <$> parseWire value

instance Wire GoalId where
  toWire = toWire . unGoalId
  parseWire value = GoalId <$> parseWire value

instance Wire MetricId where
  toWire = toWire . unMetricId
  parseWire value = MetricId <$> parseWire value

instance Wire ResourceId where
  toWire = toWire . unResourceId
  parseWire value = ResourceId <$> parseWire value

instance Wire ReviewId where
  toWire = toWire . unReviewId
  parseWire value = ReviewId <$> parseWire value

instance Wire Money where
  toWire = toWire . unMoney
  parseWire value = Money <$> parseWire value

instance Wire Organization where
  toWire Organization {..} =
    record
      [ ("id", Just (toWire organizationId))
      , ("name", Just (toWire organizationName))
      , ("createdAt", Just (toWire organizationCreatedAt))
      ]
  parseWire = withObject "Organization" $ \obj ->
    Organization
      <$> field obj "id"
      <*> field obj "name"
      <*> field obj "createdAt"

instance Wire Person where
  toWire Person {..} =
    record
      [ ("id", Just (toWire personId))
      , ("name", Just (toWire personName))
      , ("role", Just (toWire personRole))
      , ("reportsTo", toWire <$> personReportsTo)
      ]
  parseWire = withObject "Person" $ \obj ->
    Person
      <$> field obj "id"
      <*> field obj "name"
      <*> field obj "role"
      <*> optionalField obj "reportsTo"

instance Wire Metric where
  toWire Metric {..} =
    record
      [ ("id", Just (toWire metricId))
      , ("name", Just (toWire metricName))
      , ("unit", Just (toWire metricUnit))
      , ("direction", Just (toWire metricDirection))
      ]
  parseWire = withObject "Metric" $ \obj ->
    Metric
      <$> field obj "id"
      <*> field obj "name"
      <*> field obj "unit"
      <*> field obj "direction"

instance Wire Goal where
  toWire Goal {..} =
    record
      [ ("id", Just (toWire goalId))
      , ("organization", Just (toWire goalOrganization))
      , ("description", Just (toWire goalDescription))
      , ("metric", Just (toWire goalMetric))
      , ("baseline", Just (toWire goalBaseline))
      , ("target", Just (toWire goalTarget))
      , ("startsAt", Just (toWire goalStartsAt))
      , ("deadline", Just (toWire goalDeadline))
      , ("parent", toWire <$> goalParent)
      , ("requiredPermissions", Just (toWire goalRequiredPermissions))
      , ("requiredBudget", Just (toWire goalRequiredBudget))
      ]
  parseWire = withObject "Goal" $ \obj ->
    Goal
      <$> field obj "id"
      <*> field obj "organization"
      <*> field obj "description"
      <*> field obj "metric"
      <*> field obj "baseline"
      <*> field obj "target"
      <*> field obj "startsAt"
      <*> field obj "deadline"
      <*> optionalField obj "parent"
      <*> field obj "requiredPermissions"
      <*> field obj "requiredBudget"

instance Wire Ownership where
  toWire Ownership {..} =
    record
      [ ("goal", Just (toWire ownershipGoal))
      , ("owner", Just (toWire ownershipOwner))
      , ("since", Just (toWire ownershipSince))
      ]
  parseWire = withObject "Ownership" $ \obj ->
    Ownership
      <$> field obj "goal"
      <*> field obj "owner"
      <*> field obj "since"

instance Wire Authority where
  toWire Authority {..} =
    record
      [ ("owner", Just (toWire authorityOwner))
      , ("budgetLimit", Just (toWire authorityBudgetLimit))
      , ("canHire", Just (toWire authorityCanHire))
      , ("canChangePrice", Just (toWire authorityCanChangePrice))
      , ("canApprove", Just (toWire authorityCanApprove))
      ]
  parseWire = withObject "Authority" $ \obj ->
    Authority
      <$> field obj "owner"
      <*> field obj "budgetLimit"
      <*> field obj "canHire"
      <*> field obj "canChangePrice"
      <*> field obj "canApprove"

instance Wire Result where
  toWire Result {..} =
    record
      [ ("goal", Just (toWire resultGoal))
      , ("value", Just (toWire resultValue))
      , ("reportedAt", Just (toWire resultReportedAt))
      , ("reportedBy", Just (toWire resultReportedBy))
      , ("note", Just (toWire resultNote))
      ]
  parseWire = withObject "Result" $ \obj ->
    Result
      <$> field obj "goal"
      <*> field obj "value"
      <*> field obj "reportedAt"
      <*> field obj "reportedBy"
      <*> field obj "note"

instance Wire Evaluation where
  toWire Evaluation {..} =
    record
      [ ("goal", Just (toWire evaluationGoal))
      , ("status", Just (toWire evaluationStatus))
      , ("progress", Just (toWire evaluationProgress))
      , ("expectedProgress", Just (toWire evaluationExpectedProgress))
      , ("latestValue", toWire <$> evaluationLatestValue)
      , ("evaluatedAt", Just (toWire evaluationEvaluatedAt))
      ]
  parseWire = withObject "Evaluation" $ \obj ->
    Evaluation
      <$> field obj "goal"
      <*> field obj "status"
      <*> field obj "progress"
      <*> field obj "expectedProgress"
      <*> optionalField obj "latestValue"
      <*> field obj "evaluatedAt"

instance Wire Learning where
  toWire Learning {..} =
    record
      [("text", Just (toWire learningText))]
  parseWire = withObject "Learning" $ \obj ->
    Learning <$> field obj "text"

instance Wire Decision where
  toWire Decision {..} =
    record
      [ ("text", Just (toWire decisionText))
      , ("owner", Just (toWire decisionOwner))
      , ("deadline", toWire <$> decisionDeadline)
      ]
  parseWire = withObject "Decision" $ \obj ->
    Decision
      <$> field obj "text"
      <*> field obj "owner"
      <*> optionalField obj "deadline"

instance Wire Review where
  toWire Review {..} =
    record
      [ ("id", Just (toWire reviewId))
      , ("goal", Just (toWire reviewGoal))
      , ("result", toWire <$> reviewResult)
      , ("evaluation", Just (toWire reviewEvaluation))
      , ("learnings", Just (toWire reviewLearnings))
      , ("decisions", Just (toWire reviewDecisions))
      , ("heldAt", Just (toWire reviewHeldAt))
      , ("note", Just (toWire reviewNote))
      ]
  parseWire = withObject "Review" $ \obj ->
    Review
      <$> field obj "id"
      <*> field obj "goal"
      <*> optionalField obj "result"
      <*> field obj "evaluation"
      <*> field obj "learnings"
      <*> field obj "decisions"
      <*> field obj "heldAt"
      <*> field obj "note"

instance Wire StoredEvent where
  toWire StoredEvent {..} =
    record
      [ ("seq", Just (toWire storedSeq))
      , ("at", Just (toWire storedAt))
      , ("actor", toWire <$> storedActor)
      , ("event", Just (toWire storedEvent))
      ]
  parseWire = withObject "StoredEvent" $ \obj ->
    StoredEvent
      <$> field obj "seq"
      <*> field obj "at"
      <*> optionalField obj "actor"
      <*> field obj "event"

instance Wire Edge where
  toWire Edge {..} =
    record
      [ ("from", Just (toWire edgeFrom))
      , ("kind", Just (toWire edgeKind))
      , ("to", Just (toWire edgeTo))
      ]
  parseWire = withObject "Edge" $ \obj ->
    Edge
      <$> field obj "from"
      <*> field obj "kind"
      <*> field obj "to"

instance Wire ResponsibilityGraph where
  toWire ResponsibilityGraph {..} =
    record
      [ ("nodes", Just (toWire graphNodes))
      , ("edges", Just (toWire graphEdges))
      ]
  parseWire = withObject "ResponsibilityGraph" $ \obj ->
    ResponsibilityGraph
      <$> field obj "nodes"
      <*> field obj "edges"

instance Wire ResourceHolder where
  toWire ResourceHolder {..} =
    record
      [ ("resource", Just (toWire holderResource))
      , ("required", Just (toWire holderRequired))
      , ("ownerHas", Just (toWire holderOwnerHas))
      , ("controlledBy", Just (toWire holderControlledBy))
      ]
  parseWire = withObject "ResourceHolder" $ \obj ->
    ResourceHolder
      <$> field obj "resource"
      <*> field obj "required"
      <*> field obj "ownerHas"
      <*> field obj "controlledBy"

instance Wire Analysis where
  toWire Analysis {..} =
    record
      [ ("goal", Just (toWire analysisGoal))
      , ("owner", toWire <$> analysisOwner)
      , ("coverage", Just (toWire analysisCoverage))
      , ("status", toWire <$> analysisStatus)
      , ("resources", Just (toWire analysisResources))
      , ("possibleCause", Just (toWire analysisPossibleCause))
      , ("recommendations", Just (toWire analysisRecommendations))
      ]
  parseWire = withObject "Analysis" $ \obj ->
    Analysis
      <$> field obj "goal"
      <*> optionalField obj "owner"
      <*> field obj "coverage"
      <*> optionalField obj "status"
      <*> field obj "resources"
      <*> field obj "possibleCause"
      <*> field obj "recommendations"

instance Wire Diagnostic where
  toWire Diagnostic {..} =
    record
      [ ("code", Just (toWire diagnosticCode))
      , ("severity", Just (toWire diagnosticSeverity))
      , ("subject", Just (toWire diagnosticSubject))
      , ("message", Just (toWire diagnosticMessage))
      , ("details", Just (toWire diagnosticDetails))
      ]
  parseWire = withObject "Diagnostic" $ \obj ->
    Diagnostic
      <$> field obj "code"
      <*> field obj "severity"
      <*> field obj "subject"
      <*> field obj "message"
      <*> field obj "details"

instance Wire CompileReport where
  toWire CompileReport {..} =
    record
      [ ("errors", Just (toWire reportErrors))
      , ("warnings", Just (toWire reportWarnings))
      , ("infos", Just (toWire reportInfos))
      , ("diagnostics", Just (toWire reportDiagnostics))
      ]
  parseWire = withObject "CompileReport" $ \obj ->
    CompileReport
      <$> field obj "errors"
      <*> field obj "warnings"
      <*> field obj "infos"
      <*> field obj "diagnostics"

instance Wire MetricDirection where
  toWire = \case
    HigherIsBetter -> String "HigherIsBetter"
    LowerIsBetter -> String "LowerIsBetter"
  parseWire = withText "MetricDirection" $ \tag -> case tag of
    "HigherIsBetter" -> pure HigherIsBetter
    "LowerIsBetter"  -> pure LowerIsBetter
    _                -> fail ("Unknown MetricDirection: " <> T.unpack tag)

instance Wire Permission where
  toWire = \case
    Pricing -> String "Pricing"
    Hiring -> String "Hiring"
    BudgetApproval -> String "BudgetApproval"
    Contracting -> String "Contracting"
    Marketing -> String "Marketing"
    Infrastructure -> String "Infrastructure"
    ProductLaunch -> String "ProductLaunch"
  parseWire = withText "Permission" $ \tag -> case tag of
    "Pricing"        -> pure Pricing
    "Hiring"         -> pure Hiring
    "BudgetApproval" -> pure BudgetApproval
    "Contracting"    -> pure Contracting
    "Marketing"      -> pure Marketing
    "Infrastructure" -> pure Infrastructure
    "ProductLaunch"  -> pure ProductLaunch
    _                -> fail ("Unknown Permission: " <> T.unpack tag)

instance Wire GoalStatus where
  toWire = \case
    NoData -> String "NoData"
    OnTrack -> String "OnTrack"
    AtRisk -> String "AtRisk"
    OffTrack -> String "OffTrack"
    Achieved -> String "Achieved"
  parseWire = withText "GoalStatus" $ \tag -> case tag of
    "NoData"   -> pure NoData
    "OnTrack"  -> pure OnTrack
    "AtRisk"   -> pure AtRisk
    "OffTrack" -> pure OffTrack
    "Achieved" -> pure Achieved
    _          -> fail ("Unknown GoalStatus: " <> T.unpack tag)

instance Wire OrganizationError where
  toWire = \case
    NoOrganization -> object ["tag" .= ("NoOrganization" :: Text)]
    AmbiguousOrganizations -> object ["tag" .= ("AmbiguousOrganizations" :: Text)]
    OrganizationNotFound a -> object ["tag" .= ("OrganizationNotFound" :: Text), "contents" .= a]
    VersionConflict a b -> object ["tag" .= ("VersionConflict" :: Text), "contents" .= [toWire a, toWire b]]
    OrganizationAlreadyExists -> object ["tag" .= ("OrganizationAlreadyExists" :: Text)]
    GoalNotFound a -> object ["tag" .= ("GoalNotFound" :: Text), "contents" .= a]
    PersonNotFound a -> object ["tag" .= ("PersonNotFound" :: Text), "contents" .= a]
    NoOwner a -> object ["tag" .= ("NoOwner" :: Text), "contents" .= a]
    OwnerMismatch a b c ->
      object ["tag" .= ("OwnerMismatch" :: Text), "contents" .= [toWire a, toWire b, toWire c]]
    NoAuthority a -> object ["tag" .= ("NoAuthority" :: Text), "contents" .= a]
    MissingPermissions a b c ->
      object
        ["tag" .= ("MissingPermissions" :: Text), "contents" .= [toWire a, toWire b, toWire c]]
    InsufficientBudget a b c ->
      object
        ["tag" .= ("InsufficientBudget" :: Text), "contents" .= [toWire a, toWire b, toWire c]]
    InvalidTarget a -> object ["tag" .= ("InvalidTarget" :: Text), "contents" .= a]
    DeadlineBeforeStart a -> object ["tag" .= ("DeadlineBeforeStart" :: Text), "contents" .= a]
    GoalAlreadyActive a -> object ["tag" .= ("GoalAlreadyActive" :: Text), "contents" .= a]
    GoalNotActive a -> object ["tag" .= ("GoalNotActive" :: Text), "contents" .= a]
    ParentGoalNotFound a b -> object ["tag" .= ("ParentGoalNotFound" :: Text), "contents" .= [toWire a, toWire b]]
    StorageFailure -> object ["tag" .= ("StorageFailure" :: Text)]
    InvalidInput a -> object ["tag" .= ("InvalidInput" :: Text), "contents" .= a]
    DuplicateId a -> object ["tag" .= ("DuplicateId" :: Text), "contents" .= a]
  parseWire = withObject "OrganizationError" $ \obj -> do
    tag <- obj A..: "tag" :: Parser Text
    case tag of
      "NoOrganization" -> pure NoOrganization
      "AmbiguousOrganizations" -> pure AmbiguousOrganizations
      "OrganizationNotFound" -> OrganizationNotFound <$> field obj "contents"
      "VersionConflict" -> do
        values <- obj A..: "contents" :: Parser [Value]
        case values of
          [a, b] -> VersionConflict <$> parseWire a <*> parseWire b
          _      -> fail "Expected 2 constructor arguments"
      "OrganizationAlreadyExists" -> pure OrganizationAlreadyExists
      "GoalNotFound" -> GoalNotFound <$> field obj "contents"
      "PersonNotFound" -> PersonNotFound <$> field obj "contents"
      "NoOwner" -> NoOwner <$> field obj "contents"
      "OwnerMismatch" -> do
        values <- obj A..: "contents" :: Parser [Value]
        case values of
          [a, b, c] -> OwnerMismatch <$> parseWire a <*> parseWire b <*> parseWire c
          _         -> fail "Expected 3 constructor arguments"
      "NoAuthority" -> NoAuthority <$> field obj "contents"
      "MissingPermissions" -> do
        values <- obj A..: "contents" :: Parser [Value]
        case values of
          [a, b, c] -> MissingPermissions <$> parseWire a <*> parseWire b <*> parseWire c
          _         -> fail "Expected 3 constructor arguments"
      "InsufficientBudget" -> do
        values <- obj A..: "contents" :: Parser [Value]
        case values of
          [a, b, c] -> InsufficientBudget <$> parseWire a <*> parseWire b <*> parseWire c
          _         -> fail "Expected 3 constructor arguments"
      "InvalidTarget" -> InvalidTarget <$> field obj "contents"
      "DeadlineBeforeStart" -> DeadlineBeforeStart <$> field obj "contents"
      "GoalAlreadyActive" -> GoalAlreadyActive <$> field obj "contents"
      "GoalNotActive" -> GoalNotActive <$> field obj "contents"
      "ParentGoalNotFound" -> do
        values <- obj A..: "contents" :: Parser [Value]
        case values of
          [a, b] -> ParentGoalNotFound <$> parseWire a <*> parseWire b
          _      -> fail "Expected 2 constructor arguments"
      "StorageFailure" -> pure StorageFailure
      "InvalidInput" -> InvalidInput <$> field obj "contents"
      "DuplicateId" -> DuplicateId <$> field obj "contents"
      _ -> fail ("Unknown OrganizationError: " <> T.unpack tag)

instance Wire OrganizationEvent where
  toWire = \case
    OrganizationCreated a -> object ["tag" .= ("OrganizationCreated" :: Text), "contents" .= a]
    OrganizationScoped a b -> object ["tag" .= ("OrganizationScoped" :: Text), "contents" .= [toWire a, toWire b]]
    OrganizationRenamed a b -> object ["tag" .= ("OrganizationRenamed" :: Text), "contents" .= [toWire a, toWire b]]
    OrganizationDeleted a -> object ["tag" .= ("OrganizationDeleted" :: Text), "contents" .= a]
    DemoSeeded a -> object ["tag" .= ("DemoSeeded" :: Text), "contents" .= a]
    PersonAdded a -> object ["tag" .= ("PersonAdded" :: Text), "contents" .= a]
    GoalCreated a -> object ["tag" .= ("GoalCreated" :: Text), "contents" .= a]
    OwnerAssigned a b -> object ["tag" .= ("OwnerAssigned" :: Text), "contents" .= [toWire a, toWire b]]
    AuthorityGranted a b -> object ["tag" .= ("AuthorityGranted" :: Text), "contents" .= [toWire a, toWire b]]
    AuthorityRevoked a b -> object ["tag" .= ("AuthorityRevoked" :: Text), "contents" .= [toWire a, toWire b]]
    GoalActivated a -> object ["tag" .= ("GoalActivated" :: Text), "contents" .= a]
    ResultReported a b -> object ["tag" .= ("ResultReported" :: Text), "contents" .= [toWire a, toWire b]]
    GoalEvaluated a b -> object ["tag" .= ("GoalEvaluated" :: Text), "contents" .= [toWire a, toWire b]]
    ReviewHeld a -> object ["tag" .= ("ReviewHeld" :: Text), "contents" .= a]
    StrategyChanged a b -> object ["tag" .= ("StrategyChanged" :: Text), "contents" .= [toWire a, toWire b]]
  parseWire = withObject "OrganizationEvent" $ \obj -> do
    tag <- obj A..: "tag" :: Parser Text
    case tag of
      "OrganizationCreated" -> OrganizationCreated <$> field obj "contents"
      "OrganizationScoped" -> do
        values <- obj A..: "contents" :: Parser [Value]
        case values of
          [a, b] -> OrganizationScoped <$> parseWire a <*> parseWire b
          _      -> fail "Expected 2 constructor arguments"
      "OrganizationRenamed" -> do
        values <- obj A..: "contents" :: Parser [Value]
        case values of
          [a, b] -> OrganizationRenamed <$> parseWire a <*> parseWire b
          _      -> fail "Expected 2 constructor arguments"
      "OrganizationDeleted" -> OrganizationDeleted <$> field obj "contents"
      "DemoSeeded" -> DemoSeeded <$> field obj "contents"
      "PersonAdded" -> PersonAdded <$> field obj "contents"
      "GoalCreated" -> GoalCreated <$> field obj "contents"
      "OwnerAssigned" -> do
        values <- obj A..: "contents" :: Parser [Value]
        case values of
          [a, b] -> OwnerAssigned <$> parseWire a <*> parseWire b
          _      -> fail "Expected 2 constructor arguments"
      "AuthorityGranted" -> do
        values <- obj A..: "contents" :: Parser [Value]
        case values of
          [a, b] -> AuthorityGranted <$> parseWire a <*> parseWire b
          _      -> fail "Expected 2 constructor arguments"
      "AuthorityRevoked" -> do
        values <- obj A..: "contents" :: Parser [Value]
        case values of
          [a, b] -> AuthorityRevoked <$> parseWire a <*> parseWire b
          _      -> fail "Expected 2 constructor arguments"
      "GoalActivated" -> GoalActivated <$> field obj "contents"
      "ResultReported" -> do
        values <- obj A..: "contents" :: Parser [Value]
        case values of
          [a, b] -> ResultReported <$> parseWire a <*> parseWire b
          _      -> fail "Expected 2 constructor arguments"
      "GoalEvaluated" -> do
        values <- obj A..: "contents" :: Parser [Value]
        case values of
          [a, b] -> GoalEvaluated <$> parseWire a <*> parseWire b
          _      -> fail "Expected 2 constructor arguments"
      "ReviewHeld" -> ReviewHeld <$> field obj "contents"
      "StrategyChanged" -> do
        values <- obj A..: "contents" :: Parser [Value]
        case values of
          [a, b] -> StrategyChanged <$> parseWire a <*> parseWire b
          _      -> fail "Expected 2 constructor arguments"
      _ -> fail ("Unknown OrganizationEvent: " <> T.unpack tag)

instance Wire Node where
  toWire = \case
    PersonNode a -> object ["tag" .= ("PersonNode" :: Text), "contents" .= a]
    GoalNode a -> object ["tag" .= ("GoalNode" :: Text), "contents" .= a]
    MetricNode a -> object ["tag" .= ("MetricNode" :: Text), "contents" .= a]
    ResourceNode a -> object ["tag" .= ("ResourceNode" :: Text), "contents" .= a]
  parseWire = withObject "Node" $ \obj -> do
    tag <- obj A..: "tag" :: Parser Text
    case tag of
      "PersonNode"   -> PersonNode <$> field obj "contents"
      "GoalNode"     -> GoalNode <$> field obj "contents"
      "MetricNode"   -> MetricNode <$> field obj "contents"
      "ResourceNode" -> ResourceNode <$> field obj "contents"
      _              -> fail ("Unknown Node: " <> T.unpack tag)

instance Wire EdgeKind where
  toWire = \case
    Owns -> String "Owns"
    DependsOn -> String "DependsOn"
    Controls -> String "Controls"
    Measures -> String "Measures"
  parseWire = withText "EdgeKind" $ \tag -> case tag of
    "Owns"      -> pure Owns
    "DependsOn" -> pure DependsOn
    "Controls"  -> pure Controls
    "Measures"  -> pure Measures
    _           -> fail ("Unknown EdgeKind: " <> T.unpack tag)

instance Wire Recommendation where
  toWire = \case
    IncreaseOwnerAuthority a b ->
      object ["tag" .= ("IncreaseOwnerAuthority" :: Text), "contents" .= [toWire a, toWire b]]
    MoveAccountabilityUpward a -> object ["tag" .= ("MoveAccountabilityUpward" :: Text), "contents" .= a]
    AssignOwner -> object ["tag" .= ("AssignOwner" :: Text)]
    NoStructuralIssue -> object ["tag" .= ("NoStructuralIssue" :: Text)]
  parseWire = withObject "Recommendation" $ \obj -> do
    tag <- obj A..: "tag" :: Parser Text
    case tag of
      "IncreaseOwnerAuthority" -> do
        values <- obj A..: "contents" :: Parser [Value]
        case values of
          [a, b] -> IncreaseOwnerAuthority <$> parseWire a <*> parseWire b
          _      -> fail "Expected 2 constructor arguments"
      "MoveAccountabilityUpward" -> MoveAccountabilityUpward <$> field obj "contents"
      "AssignOwner" -> pure AssignOwner
      "NoStructuralIssue" -> pure NoStructuralIssue
      _ -> fail ("Unknown Recommendation: " <> T.unpack tag)

instance Wire Severity where
  toWire = \case
    Error -> String "Error"
    Warning -> String "Warning"
    Info -> String "Info"
  parseWire = withText "Severity" $ \tag -> case tag of
    "Error"   -> pure Error
    "Warning" -> pure Warning
    "Info"    -> pure Info
    _         -> fail ("Unknown Severity: " <> T.unpack tag)

instance Wire ReviewWarning where
  toWire = \case
    NoDecisionProduced -> object ["tag" .= ("NoDecisionProduced" :: Text)]
    DecisionWithoutDeadline a -> object ["tag" .= ("DecisionWithoutDeadline" :: Text), "contents" .= a]
  parseWire = withObject "ReviewWarning" $ \obj -> do
    tag <- obj A..: "tag" :: Parser Text
    case tag of
      "NoDecisionProduced"      -> pure NoDecisionProduced
      "DecisionWithoutDeadline" -> DecisionWithoutDeadline <$> field obj "contents"
      _                         -> fail ("Unknown ReviewWarning: " <> T.unpack tag)

-- Diagnostics are a presentation projection, not a persisted event. Decoding
-- historical message text preserves the wire value but cannot recover its cause.
instance Wire DiagnosticMessage where
  toWire = toWire . renderDiagnosticMessage
  parseWire value = PlainMessage <$> parseWire value
