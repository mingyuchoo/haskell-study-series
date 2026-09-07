-- | Explicit Event JSON contract. Keys and tags are stable.
module MyOrg.Serialization.Event
  ( organizationEventCodec
  , storedEventCodec
  ) where

import Data.Aeson (withObject)
import Data.Text qualified as T
import MyOrg.Domain.Event.Types
import MyOrg.Serialization.Authority
import MyOrg.Serialization.Codec
import MyOrg.Serialization.Goal
import MyOrg.Serialization.Identity
import MyOrg.Serialization.Organization
import MyOrg.Serialization.Result
import MyOrg.Serialization.Review

storedEventCodec :: Codec StoredEvent
storedEventCodec = Codec encode decode
  where
    encode StoredEvent {..} = record
      [ ("seq", Just (encodeValue intCodec storedSeq))
      , ("at", Just (encodeValue timeCodec storedAt))
      , ("actor", encodeValue userIdCodec <$> storedActor)
      , ("event", Just (encodeValue organizationEventCodec storedEvent))
      ]
    decode = withObject "StoredEvent" $ \obj ->
      StoredEvent
        <$> field intCodec obj "seq"
        <*> field timeCodec obj "at"
        <*> optionalField userIdCodec obj "actor"
        <*> field organizationEventCodec obj "event"

organizationEventCodec :: Codec OrganizationEvent
organizationEventCodec = Codec encode decode
  where
    encode = \case
      OrganizationCreated a -> tagged "OrganizationCreated" (Just (encodeValue organizationCodec a))
      OrganizationScoped a b -> tagged "OrganizationScoped" (Just (encodeValue (listCodec valueCodec) [encodeValue orgIdCodec a, encodeValue organizationEventCodec b]))
      OrganizationRenamed a b -> tagged "OrganizationRenamed" (Just (encodeValue (listCodec valueCodec) [encodeValue orgIdCodec a, encodeValue textCodec b]))
      OrganizationDeleted a -> tagged "OrganizationDeleted" (Just (encodeValue orgIdCodec a))
      DemoSeeded a -> tagged "DemoSeeded" (Just (encodeValue orgIdCodec a))
      PersonAdded a -> tagged "PersonAdded" (Just (encodeValue personCodec a))
      EmployeeAdded a b -> tagged "EmployeeAdded" (Just (encodeValue (listCodec valueCodec) [encodeValue personCodec a, encodeValue employeeProfileCodec b]))
      PersonUpdated a b -> tagged "PersonUpdated" (Just (encodeValue (listCodec valueCodec) [encodeValue personCodec a, encodeValue employeeProfileCodec b]))
      PersonDeactivated a b -> tagged "PersonDeactivated" (Just (encodeValue (listCodec valueCodec) [encodeValue userIdCodec a, encodeValue (maybeCodec userIdCodec) b]))
      GoalCreated a -> tagged "GoalCreated" (Just (encodeValue goalCodec a))
      OwnerAssigned a b -> tagged "OwnerAssigned" (Just (encodeValue (listCodec valueCodec) [encodeValue goalIdCodec a, encodeValue userIdCodec b]))
      AuthorityGranted a b -> tagged "AuthorityGranted" (Just (encodeValue (listCodec valueCodec) [encodeValue userIdCodec a, encodeValue authorityCodec b]))
      AuthorityRevoked a b -> tagged "AuthorityRevoked" (Just (encodeValue (listCodec valueCodec) [encodeValue userIdCodec a, encodeValue permissionCodec b]))
      GoalActivated a -> tagged "GoalActivated" (Just (encodeValue goalIdCodec a))
      ResultReported a b -> tagged "ResultReported" (Just (encodeValue (listCodec valueCodec) [encodeValue goalIdCodec a, encodeValue resultCodec b]))
      GoalEvaluated a b -> tagged "GoalEvaluated" (Just (encodeValue (listCodec valueCodec) [encodeValue goalIdCodec a, encodeValue evaluationCodec b]))
      ReviewHeld a -> tagged "ReviewHeld" (Just (encodeValue reviewCodec a))
      StrategyChanged a b -> tagged "StrategyChanged" (Just (encodeValue (listCodec valueCodec) [encodeValue goalIdCodec a, encodeValue textCodec b]))
    decode = withObject "OrganizationEvent" $ \obj -> do
      tag <- field textCodec obj "tag"
      case tag of
        "OrganizationCreated" -> OrganizationCreated <$> field organizationCodec obj "contents"
        "OrganizationScoped" -> do
          values <- field (listCodec valueCodec) obj "contents"
          case values of
            [a, b] -> OrganizationScoped <$> decodeValue orgIdCodec a <*> decodeValue organizationEventCodec b
            _ -> fail "Expected 2 constructor arguments"
        "OrganizationRenamed" -> do
          values <- field (listCodec valueCodec) obj "contents"
          case values of
            [a, b] -> OrganizationRenamed <$> decodeValue orgIdCodec a <*> decodeValue textCodec b
            _ -> fail "Expected 2 constructor arguments"
        "OrganizationDeleted" -> OrganizationDeleted <$> field orgIdCodec obj "contents"
        "DemoSeeded" -> DemoSeeded <$> field orgIdCodec obj "contents"
        "PersonAdded" -> PersonAdded <$> field personCodec obj "contents"
        "EmployeeAdded" -> do
          values <- field (listCodec valueCodec) obj "contents"
          case values of
            [a, b] -> EmployeeAdded <$> decodeValue personCodec a <*> decodeValue employeeProfileCodec b
            _ -> fail "Expected 2 constructor arguments"
        "PersonUpdated" -> do
          values <- field (listCodec valueCodec) obj "contents"
          case values of
            [a, b] -> PersonUpdated <$> decodeValue personCodec a <*> decodeValue employeeProfileCodec b
            _ -> fail "Expected 2 constructor arguments"
        "PersonDeactivated" -> do
          values <- field (listCodec valueCodec) obj "contents"
          case values of
            [a, b] -> PersonDeactivated <$> decodeValue userIdCodec a <*> decodeValue (maybeCodec userIdCodec) b
            _ -> fail "Expected 2 constructor arguments"
        "GoalCreated" -> GoalCreated <$> field goalCodec obj "contents"
        "OwnerAssigned" -> do
          values <- field (listCodec valueCodec) obj "contents"
          case values of
            [a, b] -> OwnerAssigned <$> decodeValue goalIdCodec a <*> decodeValue userIdCodec b
            _ -> fail "Expected 2 constructor arguments"
        "AuthorityGranted" -> do
          values <- field (listCodec valueCodec) obj "contents"
          case values of
            [a, b] -> AuthorityGranted <$> decodeValue userIdCodec a <*> decodeValue authorityCodec b
            _ -> fail "Expected 2 constructor arguments"
        "AuthorityRevoked" -> do
          values <- field (listCodec valueCodec) obj "contents"
          case values of
            [a, b] -> AuthorityRevoked <$> decodeValue userIdCodec a <*> decodeValue permissionCodec b
            _ -> fail "Expected 2 constructor arguments"
        "GoalActivated" -> GoalActivated <$> field goalIdCodec obj "contents"
        "ResultReported" -> do
          values <- field (listCodec valueCodec) obj "contents"
          case values of
            [a, b] -> ResultReported <$> decodeValue goalIdCodec a <*> decodeValue resultCodec b
            _ -> fail "Expected 2 constructor arguments"
        "GoalEvaluated" -> do
          values <- field (listCodec valueCodec) obj "contents"
          case values of
            [a, b] -> GoalEvaluated <$> decodeValue goalIdCodec a <*> decodeValue evaluationCodec b
            _ -> fail "Expected 2 constructor arguments"
        "ReviewHeld" -> ReviewHeld <$> field reviewCodec obj "contents"
        "StrategyChanged" -> do
          values <- field (listCodec valueCodec) obj "contents"
          case values of
            [a, b] -> StrategyChanged <$> decodeValue goalIdCodec a <*> decodeValue textCodec b
            _ -> fail "Expected 2 constructor arguments"
        _ -> fail ("Unknown OrganizationEvent: " <> T.unpack tag)
