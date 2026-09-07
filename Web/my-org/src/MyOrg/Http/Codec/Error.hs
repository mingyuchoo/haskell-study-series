-- | Explicit Error JSON contract. Keys and tags are stable.
module MyOrg.Http.Codec.Error
  ( organizationErrorCodec
  ) where

import Data.Aeson (withObject)
import Data.Text qualified as T
import MyOrg.Domain.Error
import MyOrg.Serialization.Authority
import MyOrg.Serialization.Codec
import MyOrg.Serialization.Identity

organizationErrorCodec :: Codec OrganizationError
organizationErrorCodec = Codec encode decode
  where
    encode = \case
      NoOrganization -> tagged "NoOrganization" Nothing
      AmbiguousOrganizations -> tagged "AmbiguousOrganizations" Nothing
      OrganizationNotFound a -> tagged "OrganizationNotFound" (Just (encodeValue orgIdCodec a))
      VersionConflict a b ->
        tagged
          "VersionConflict"
          (Just (encodeValue (listCodec valueCodec) [encodeValue intCodec a, encodeValue intCodec b]))
      OrganizationAlreadyExists -> tagged "OrganizationAlreadyExists" Nothing
      GoalNotFound a -> tagged "GoalNotFound" (Just (encodeValue goalIdCodec a))
      PersonNotFound a -> tagged "PersonNotFound" (Just (encodeValue userIdCodec a))
      NoOwner a -> tagged "NoOwner" (Just (encodeValue goalIdCodec a))
      OwnerMismatch a b c ->
        tagged
          "OwnerMismatch"
          ( Just
              ( encodeValue
                  (listCodec valueCodec)
                  [encodeValue goalIdCodec a, encodeValue userIdCodec b, encodeValue userIdCodec c]
              )
          )
      NoAuthority a -> tagged "NoAuthority" (Just (encodeValue userIdCodec a))
      MissingPermissions a b c ->
        tagged
          "MissingPermissions"
          ( Just
              ( encodeValue
                  (listCodec valueCodec)
                  [ encodeValue goalIdCodec a
                  , encodeValue userIdCodec b
                  , encodeValue (setCodec permissionCodec) c
                  ]
              )
          )
      InsufficientBudget a b c ->
        tagged
          "InsufficientBudget"
          ( Just
              ( encodeValue
                  (listCodec valueCodec)
                  [encodeValue goalIdCodec a, encodeValue moneyCodec b, encodeValue moneyCodec c]
              )
          )
      InvalidTarget a -> tagged "InvalidTarget" (Just (encodeValue goalIdCodec a))
      DeadlineBeforeStart a -> tagged "DeadlineBeforeStart" (Just (encodeValue goalIdCodec a))
      GoalAlreadyActive a -> tagged "GoalAlreadyActive" (Just (encodeValue goalIdCodec a))
      GoalNotActive a -> tagged "GoalNotActive" (Just (encodeValue goalIdCodec a))
      ParentGoalNotFound a b ->
        tagged
          "ParentGoalNotFound"
          ( Just
              (encodeValue (listCodec valueCodec) [encodeValue goalIdCodec a, encodeValue goalIdCodec b])
          )
      StorageFailure -> tagged "StorageFailure" Nothing
      InvalidInput a -> tagged "InvalidInput" (Just (encodeValue textCodec a))
      DuplicateId a -> tagged "DuplicateId" (Just (encodeValue textCodec a))
    decode = withObject "OrganizationError" $ \obj -> do
      tag <- field textCodec obj "tag"
      case tag of
        "NoOrganization" -> pure NoOrganization
        "AmbiguousOrganizations" -> pure AmbiguousOrganizations
        "OrganizationNotFound" -> OrganizationNotFound <$> field orgIdCodec obj "contents"
        "VersionConflict" -> do
          values <- field (listCodec valueCodec) obj "contents"
          case values of
            [a, b] -> VersionConflict <$> decodeValue intCodec a <*> decodeValue intCodec b
            _ -> fail "Expected 2 constructor arguments"
        "OrganizationAlreadyExists" -> pure OrganizationAlreadyExists
        "GoalNotFound" -> GoalNotFound <$> field goalIdCodec obj "contents"
        "PersonNotFound" -> PersonNotFound <$> field userIdCodec obj "contents"
        "NoOwner" -> NoOwner <$> field goalIdCodec obj "contents"
        "OwnerMismatch" -> do
          values <- field (listCodec valueCodec) obj "contents"
          case values of
            [a, b, c] ->
              OwnerMismatch
                <$> decodeValue goalIdCodec a
                <*> decodeValue userIdCodec b
                <*> decodeValue userIdCodec c
            _ -> fail "Expected 3 constructor arguments"
        "NoAuthority" -> NoAuthority <$> field userIdCodec obj "contents"
        "MissingPermissions" -> do
          values <- field (listCodec valueCodec) obj "contents"
          case values of
            [a, b, c] ->
              MissingPermissions
                <$> decodeValue goalIdCodec a
                <*> decodeValue userIdCodec b
                <*> decodeValue (setCodec permissionCodec) c
            _ -> fail "Expected 3 constructor arguments"
        "InsufficientBudget" -> do
          values <- field (listCodec valueCodec) obj "contents"
          case values of
            [a, b, c] ->
              InsufficientBudget
                <$> decodeValue goalIdCodec a
                <*> decodeValue moneyCodec b
                <*> decodeValue moneyCodec c
            _ -> fail "Expected 3 constructor arguments"
        "InvalidTarget" -> InvalidTarget <$> field goalIdCodec obj "contents"
        "DeadlineBeforeStart" -> DeadlineBeforeStart <$> field goalIdCodec obj "contents"
        "GoalAlreadyActive" -> GoalAlreadyActive <$> field goalIdCodec obj "contents"
        "GoalNotActive" -> GoalNotActive <$> field goalIdCodec obj "contents"
        "ParentGoalNotFound" -> do
          values <- field (listCodec valueCodec) obj "contents"
          case values of
            [a, b] -> ParentGoalNotFound <$> decodeValue goalIdCodec a <*> decodeValue goalIdCodec b
            _ -> fail "Expected 2 constructor arguments"
        "StorageFailure" -> pure StorageFailure
        "InvalidInput" -> InvalidInput <$> field textCodec obj "contents"
        "DuplicateId" -> DuplicateId <$> field textCodec obj "contents"
        _ -> fail ("Unknown OrganizationError: " <> T.unpack tag)
