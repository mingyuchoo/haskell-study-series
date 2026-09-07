-- | Stable survey wire and persistence contract.
module MyOrg.Serialization.Discovery
  ( discoveryCodec
  , knowledgeCodec
  ) where

import Data.Aeson (withObject)
import Data.Maybe (fromMaybe)
import MyOrg.Domain.Discovery
import MyOrg.Serialization.Authority (permissionCodec)
import MyOrg.Serialization.Codec
import MyOrg.Serialization.Identity (userIdCodec)

knowledgeCodec :: Codec KnowledgeStatus
knowledgeCodec = Codec encode decode
  where
    encode =
      encodeValue textCodec
        . (\case Confirmed -> "confirmed"; Unknown -> "unknown"; Proposed -> "proposed")
    decode value =
      decodeValue textCodec value >>= \case
        "confirmed" -> pure Confirmed
        "unknown" -> pure Unknown
        "proposed" -> pure Proposed
        _ -> fail "상태는 confirmed, unknown, proposed 중 하나여야 합니다."

reviewStatusCodec :: Codec DiscoveryReviewStatus
reviewStatusCodec = Codec encode decode
  where
    encode = encodeValue textCodec . (\case Pending -> "pending"; Reviewed -> "reviewed")
    decode value =
      decodeValue textCodec value >>= \case
        "pending" -> pure Pending
        "reviewed" -> pure Reviewed
        _ -> fail "검토 상태는 pending 또는 reviewed여야 합니다."

observationCodec :: Codec Observation
observationCodec = Codec encode decode
  where
    encode Observation {..} =
      record
        [ ("id", Just (encodeValue textCodec observationId))
        , ("subject", Just (encodeValue textCodec observationSubject))
        , ("detail", Just (encodeValue textCodec observationDetail))
        , ("status", Just (encodeValue knowledgeCodec observationStatus))
        , ("evidence", Just (encodeValue textCodec observationEvidence))
        ]
    decode = withObject "Observation" $ \o ->
      Observation
        <$> field textCodec o "id"
        <*> field textCodec o "subject"
        <*> field textCodec o "detail"
        <*> field knowledgeCodec o "status"
        <*> field textCodec o "evidence"

workflowCodec :: Codec Workflow
workflowCodec = Codec encode decode
  where
    encode Workflow {..} =
      record
        [ ("id", Just (encodeValue textCodec workflowId))
        , ("name", Just (encodeValue textCodec workflowName))
        , ("role", Just (encodeValue textCodec workflowRole))
        , ("rolePerson", encodeValue userIdCodec <$> workflowRolePerson)
        , ("trigger", Just (encodeValue textCodec workflowTrigger))
        , ("inputs", Just (encodeValue textCodec workflowInputs))
        , ("tools", Just (encodeValue textCodec workflowTools))
        , ("outputs", Just (encodeValue textCodec workflowOutputs))
        , ("handoff", Just (encodeValue textCodec workflowHandoff))
        ,
          ( "handoffWorkflows"
          , if null workflowHandoffWorkflows
              then Nothing
              else Just (encodeValue (listCodec textCodec) workflowHandoffWorkflows)
          )
        , ("approval", Just (encodeValue textCodec workflowApproval))
        , ("approvalPerson", encodeValue userIdCodec <$> workflowApprovalPerson)
        , ("approvalPermission", encodeValue permissionCodec <$> workflowApprovalPermission)
        , ("status", Just (encodeValue knowledgeCodec workflowStatus))
        , ("evidence", Just (encodeValue textCodec workflowEvidence))
        ]
    decode = withObject "Workflow" $ \o ->
      Workflow
        <$> field textCodec o "id"
        <*> field textCodec o "name"
        <*> field textCodec o "role"
        <*> optionalField userIdCodec o "rolePerson"
        <*> field textCodec o "trigger"
        <*> field textCodec o "inputs"
        <*> field textCodec o "tools"
        <*> field textCodec o "outputs"
        <*> field textCodec o "handoff"
        <*> (fromMaybe [] <$> optionalField (listCodec textCodec) o "handoffWorkflows")
        <*> field textCodec o "approval"
        <*> optionalField userIdCodec o "approvalPerson"
        <*> optionalField permissionCodec o "approvalPermission"
        <*> field knowledgeCodec o "status"
        <*> field textCodec o "evidence"

reviewCodec :: Codec DiscoveryReview
reviewCodec = Codec encode decode
  where
    encode DiscoveryReview {..} =
      record
        [ ("status", Just (encodeValue reviewStatusCodec discoveryReviewStatus))
        , ("note", Just (encodeValue textCodec discoveryReviewNote))
        ]
    decode = withObject "DiscoveryReview" $ \o ->
      DiscoveryReview
        <$> field reviewStatusCodec o "status"
        <*> field textCodec o "note"

discoveryCodec :: Codec Discovery
discoveryCodec = Codec encode decode
  where
    encode Discovery {..} =
      record
        [ ("scope", Just (encodeValue textCodec discoveryScope))
        , ("asOf", Just (encodeValue textCodec discoveryAsOf))
        , ("observations", Just (encodeValue (listCodec observationCodec) discoveryObservations))
        , ("workflows", Just (encodeValue (listCodec workflowCodec) discoveryWorkflows))
        , ("review", Just (encodeValue reviewCodec discoveryReview))
        ]
    decode = withObject "Discovery" $ \o ->
      Discovery
        <$> field textCodec o "scope"
        <*> field textCodec o "asOf"
        <*> field (listCodec observationCodec) o "observations"
        <*> field (listCodec workflowCodec) o "workflows"
        <*> field reviewCodec o "review"
