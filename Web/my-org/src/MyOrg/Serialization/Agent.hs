-- | Stable agent design wire and persistence contract.
module MyOrg.Serialization.Agent
  ( agentRoleCodec
  , permissionLevelCodec
  , approvalByCodec
  ) where

import Data.Aeson (Value (String), withObject, withText)
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as T
import MyOrg.Domain.Agent
import MyOrg.Serialization.Authority (permissionCodec)
import MyOrg.Serialization.Codec
import MyOrg.Serialization.Discovery (knowledgeCodec)
import MyOrg.Serialization.Identity (userIdCodec)

permissionLevelCodec :: Codec PermissionLevel
permissionLevelCodec = Codec encode decode
  where
    encode = \case
      L0Read -> String "L0"
      L1Workspace -> String "L1"
      L2External -> String "L2"
      L3Forbidden -> String "L3"
    decode = withText "PermissionLevel" $ \tag -> case tag of
      "L0" -> pure L0Read
      "L1" -> pure L1Workspace
      "L2" -> pure L2External
      "L3" -> pure L3Forbidden
      _    -> fail ("권한 등급은 L0, L1, L2, L3 중 하나여야 합니다: " <> T.unpack tag)

approvalByCodec :: Codec ApprovalBy
approvalByCodec = Codec encode decode
  where
    encode = \case
      ApprovalPerson uid -> record [("person", Just (encodeValue userIdCodec uid))]
      ApprovalPermission p -> record [("permission", Just (encodeValue permissionCodec p))]
    decode = withObject "ApprovalBy" $ \o ->
      case (KM.lookup "person" o, KM.lookup "permission" o) of
        (Just person, Nothing) -> ApprovalPerson <$> decodeValue userIdCodec person
        (Nothing, Just permission) -> ApprovalPermission <$> decodeValue permissionCodec permission
        _ -> fail "승인 주체는 person 또는 permission 중 하나여야 합니다."

agentRoleCodec :: Codec AgentRole
agentRoleCodec = Codec encode decode
  where
    encode AgentRole {..} =
      record
        [ ("id", Just (encodeValue textCodec agentId))
        , ("name", Just (encodeValue textCodec agentName))
        , ("sourceWorkflow", encodeValue textCodec <$> agentSourceWorkflow)
        , ("task", Just (encodeValue textCodec agentTask))
        , ("inputs", Just (encodeValue textCodec agentInputs))
        , ("outputs", Just (encodeValue textCodec agentOutputs))
        , ("tools", Just (encodeValue (listCodec textCodec) agentTools))
        , ("permissionLevel", Just (encodeValue permissionLevelCodec agentPermissionLevel))
        , ("approvalBy", encodeValue approvalByCodec <$> agentApprovalBy)
        , ("handoffTo", Just (encodeValue (listCodec textCodec) agentHandoffTo))
        , ("status", Just (encodeValue knowledgeCodec agentStatus))
        , ("evidence", Just (encodeValue textCodec agentEvidence))
        ]
    decode = withObject "AgentRole" $ \o ->
      AgentRole
        <$> field textCodec o "id"
        <*> field textCodec o "name"
        <*> optionalField textCodec o "sourceWorkflow"
        <*> field textCodec o "task"
        <*> field textCodec o "inputs"
        <*> field textCodec o "outputs"
        <*> field (listCodec textCodec) o "tools"
        <*> field permissionLevelCodec o "permissionLevel"
        <*> optionalField approvalByCodec o "approvalBy"
        <*> field (listCodec textCodec) o "handoffTo"
        <*> field knowledgeCodec o "status"
        <*> field textCodec o "evidence"
