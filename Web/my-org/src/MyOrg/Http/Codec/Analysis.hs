-- | Explicit Analysis JSON contract. Keys and tags are stable.
module MyOrg.Http.Codec.Analysis
  ( resourceHolderViewCodec
  , analysisViewCodec
  , recommendationViewCodec
  ) where

import Data.Aeson (withObject)
import Data.Text qualified as T
import MyOrg.Presentation.Analysis qualified as Analysis
import MyOrg.Serialization.Codec
import MyOrg.Serialization.Identity
import MyOrg.Serialization.Result

resourceHolderViewCodec :: Codec Analysis.ResourceHolderView
resourceHolderViewCodec = Codec encode decode
  where
    encode Analysis.ResourceHolderView {..} =
      record
        [ ("resource", Just (encodeValue textCodec resource))
        , ("required", Just (encodeValue boolCodec required))
        , ("ownerHas", Just (encodeValue boolCodec ownerHas))
        , ("controlledBy", Just (encodeValue (listCodec userIdCodec) controlledBy))
        ]
    decode = withObject "ResourceHolderView" $ \obj ->
      Analysis.ResourceHolderView
        <$> field textCodec obj "resource"
        <*> field boolCodec obj "required"
        <*> field boolCodec obj "ownerHas"
        <*> field (listCodec userIdCodec) obj "controlledBy"

analysisViewCodec :: Codec Analysis.AnalysisView
analysisViewCodec = Codec encode decode
  where
    encode Analysis.AnalysisView {..} =
      record
        [ ("goal", Just (encodeValue goalIdCodec goal))
        , ("owner", encodeValue userIdCodec <$> owner)
        , ("coverage", Just (encodeValue doubleCodec coverage))
        , ("status", encodeValue goalStatusCodec <$> status)
        , ("resources", Just (encodeValue (listCodec resourceHolderViewCodec) resources))
        , ("possibleCause", Just (encodeValue textCodec possibleCause))
        ,
          ( "recommendations"
          , Just (encodeValue (listCodec recommendationViewCodec) recommendations)
          )
        ]
    decode = withObject "AnalysisView" $ \obj ->
      Analysis.AnalysisView
        <$> field goalIdCodec obj "goal"
        <*> optionalField userIdCodec obj "owner"
        <*> field doubleCodec obj "coverage"
        <*> optionalField goalStatusCodec obj "status"
        <*> field (listCodec resourceHolderViewCodec) obj "resources"
        <*> field textCodec obj "possibleCause"
        <*> field (listCodec recommendationViewCodec) obj "recommendations"

recommendationViewCodec :: Codec Analysis.RecommendationView
recommendationViewCodec = Codec encode decode
  where
    encode = \case
      Analysis.IncreaseAuthorityView a b ->
        tagged
          "IncreaseOwnerAuthority"
          ( Just
              ( encodeValue
                  (listCodec valueCodec)
                  [encodeValue userIdCodec a, encodeValue (listCodec textCodec) b]
              )
          )
      Analysis.MoveAccountabilityView a -> tagged "MoveAccountabilityUpward" (Just (encodeValue userIdCodec a))
      Analysis.AssignOwnerView -> tagged "AssignOwner" Nothing
      Analysis.NoStructuralIssueView -> tagged "NoStructuralIssue" Nothing
    decode = withObject "Analysis.RecommendationView" $ \obj -> do
      tag <- field textCodec obj "tag"
      case tag of
        "IncreaseOwnerAuthority" -> do
          values <- field (listCodec valueCodec) obj "contents"
          case values of
            [a, b] ->
              Analysis.IncreaseAuthorityView
                <$> decodeValue userIdCodec a
                <*> decodeValue (listCodec textCodec) b
            _ -> fail "Expected 2 constructor arguments"
        "MoveAccountabilityUpward" -> Analysis.MoveAccountabilityView <$> field userIdCodec obj "contents"
        "AssignOwner" -> pure Analysis.AssignOwnerView
        "NoStructuralIssue" -> pure Analysis.NoStructuralIssueView
        _ -> fail ("Unknown Analysis.RecommendationView: " <> T.unpack tag)
