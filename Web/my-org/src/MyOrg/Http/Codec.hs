-- | Public HTTP wire values. Persistence uses its own explicit entry point.
-- Domain types have no Aeson instances. This module owns both the Wire class
-- and its instances, and the WireValue wrapper owns the only Aeson instances.
module MyOrg.Http.Codec
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

import Data.Aeson (Key, Object, Value (..), object, withArray, withObject)
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Pair, Parser)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable qualified as F
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (UTCTime)
import MyOrg.Domain.Agent
import MyOrg.Domain.Authority
import MyOrg.Domain.Compiler
import MyOrg.Domain.Discovery
import MyOrg.Domain.Error
import MyOrg.Domain.Event.Types
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Graph
import MyOrg.Domain.Identity
import MyOrg.Domain.Organization
import MyOrg.Domain.Result
import MyOrg.Domain.Review
import MyOrg.Domain.Review.Types
import MyOrg.Presentation.Analysis qualified as Analysis
import MyOrg.Presentation.Diagnostic qualified as Diagnostic
import MyOrg.Serialization.Codec (decodeValue, encodeValue)

import MyOrg.Http.Codec.Analysis
import MyOrg.Http.Codec.Diagnostic
import MyOrg.Http.Codec.Error
import MyOrg.Http.Codec.Graph
import MyOrg.Http.Codec.Review
import MyOrg.Serialization.Agent
import MyOrg.Serialization.Authority
import MyOrg.Serialization.Discovery
import MyOrg.Serialization.Event
import MyOrg.Serialization.Goal
import MyOrg.Serialization.Identity
import MyOrg.Serialization.Organization
import MyOrg.Serialization.Result
import MyOrg.Serialization.Review

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
  toWire = encodeValue orgIdCodec
  parseWire = decodeValue orgIdCodec

instance Wire UserId where
  toWire = encodeValue userIdCodec
  parseWire = decodeValue userIdCodec

instance Wire GoalId where
  toWire = encodeValue goalIdCodec
  parseWire = decodeValue goalIdCodec

instance Wire MetricId where
  toWire = encodeValue metricIdCodec
  parseWire = decodeValue metricIdCodec

instance Wire ResourceId where
  toWire = encodeValue resourceIdCodec
  parseWire = decodeValue resourceIdCodec

instance Wire ReviewId where
  toWire = encodeValue reviewIdCodec
  parseWire = decodeValue reviewIdCodec

instance Wire Money where
  toWire = encodeValue moneyCodec
  parseWire = decodeValue moneyCodec

instance Wire Organization where
  toWire = encodeValue organizationCodec
  parseWire = decodeValue organizationCodec

instance Wire Person where
  toWire = encodeValue personCodec
  parseWire = decodeValue personCodec

instance Wire EmployeeProfile where
  toWire = encodeValue employeeProfileCodec
  parseWire = decodeValue employeeProfileCodec

instance Wire MetricDirection where
  toWire = encodeValue metricDirectionCodec
  parseWire = decodeValue metricDirectionCodec

instance Wire Metric where
  toWire = encodeValue metricCodec
  parseWire = decodeValue metricCodec

instance Wire Goal where
  toWire = encodeValue goalCodec
  parseWire = decodeValue goalCodec

instance Wire Permission where
  toWire = encodeValue permissionCodec
  parseWire = decodeValue permissionCodec

instance Wire Ownership where
  toWire = encodeValue ownershipCodec
  parseWire = decodeValue ownershipCodec

instance Wire Authority where
  toWire = encodeValue authorityCodec
  parseWire = decodeValue authorityCodec

instance Wire GoalStatus where
  toWire = encodeValue goalStatusCodec
  parseWire = decodeValue goalStatusCodec

instance Wire Result where
  toWire = encodeValue resultCodec
  parseWire = decodeValue resultCodec

instance Wire Evaluation where
  toWire = encodeValue evaluationCodec
  parseWire = decodeValue evaluationCodec

instance Wire Learning where
  toWire = encodeValue learningCodec
  parseWire = decodeValue learningCodec

instance Wire Decision where
  toWire = encodeValue decisionCodec
  parseWire = decodeValue decisionCodec

instance Wire Review where
  toWire = encodeValue reviewCodec
  parseWire = decodeValue reviewCodec

instance Wire Discovery where
  toWire = encodeValue discoveryCodec
  parseWire = decodeValue discoveryCodec

instance Wire AgentRole where
  toWire = encodeValue agentRoleCodec
  parseWire = decodeValue agentRoleCodec

instance Wire PermissionLevel where
  toWire = encodeValue permissionLevelCodec
  parseWire = decodeValue permissionLevelCodec

instance Wire ApprovalBy where
  toWire = encodeValue approvalByCodec
  parseWire = decodeValue approvalByCodec

instance Wire OrganizationEvent where
  toWire = encodeValue organizationEventCodec
  parseWire = decodeValue organizationEventCodec

instance Wire StoredEvent where
  toWire = encodeValue storedEventCodec
  parseWire = decodeValue storedEventCodec

instance Wire Node where
  toWire = encodeValue nodeCodec
  parseWire = decodeValue nodeCodec

instance Wire EdgeKind where
  toWire = encodeValue edgeKindCodec
  parseWire = decodeValue edgeKindCodec

instance Wire Edge where
  toWire = encodeValue edgeCodec
  parseWire = decodeValue edgeCodec

instance Wire ResponsibilityGraph where
  toWire = encodeValue responsibilityGraphCodec
  parseWire = decodeValue responsibilityGraphCodec

instance Wire OrganizationError where
  toWire = encodeValue organizationErrorCodec
  parseWire = decodeValue organizationErrorCodec

instance Wire Analysis.ResourceHolderView where
  toWire = encodeValue resourceHolderViewCodec
  parseWire = decodeValue resourceHolderViewCodec

instance Wire Analysis.AnalysisView where
  toWire = encodeValue analysisViewCodec
  parseWire = decodeValue analysisViewCodec

instance Wire Analysis.RecommendationView where
  toWire = encodeValue recommendationViewCodec
  parseWire = decodeValue recommendationViewCodec

instance Wire Diagnostic.DiagnosticView where
  toWire = encodeValue diagnosticViewCodec
  parseWire = decodeValue diagnosticViewCodec

instance Wire Diagnostic.CompileReportView where
  toWire = encodeValue compileReportViewCodec
  parseWire = decodeValue compileReportViewCodec

instance Wire Severity where
  toWire = encodeValue severityCodec
  parseWire = decodeValue severityCodec

instance Wire ReviewWarning where
  toWire = encodeValue reviewWarningCodec
  parseWire = decodeValue reviewWarningCodec
