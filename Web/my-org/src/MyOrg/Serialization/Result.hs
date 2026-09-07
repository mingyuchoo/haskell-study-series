-- | Explicit Result JSON contract. Keys and tags are stable.
module MyOrg.Serialization.Result
  ( goalStatusCodec
  , resultCodec
  , evaluationCodec
  ) where

import Data.Aeson (Value (String), withObject, withText)
import Data.Text qualified as T
import MyOrg.Domain.Result
import MyOrg.Serialization.Codec
import MyOrg.Serialization.Identity

resultCodec :: Codec Result
resultCodec = Codec encode decode
  where
    encode Result {..} =
      record
        [ ("goal", Just (encodeValue goalIdCodec resultGoal))
        , ("value", Just (encodeValue doubleCodec resultValue))
        , ("reportedAt", Just (encodeValue timeCodec resultReportedAt))
        , ("reportedBy", Just (encodeValue userIdCodec resultReportedBy))
        , ("note", Just (encodeValue textCodec resultNote))
        ]
    decode = withObject "Result" $ \obj ->
      Result
        <$> field goalIdCodec obj "goal"
        <*> field doubleCodec obj "value"
        <*> field timeCodec obj "reportedAt"
        <*> field userIdCodec obj "reportedBy"
        <*> field textCodec obj "note"

evaluationCodec :: Codec Evaluation
evaluationCodec = Codec encode decode
  where
    encode Evaluation {..} =
      record
        [ ("goal", Just (encodeValue goalIdCodec evaluationGoal))
        , ("status", Just (encodeValue goalStatusCodec evaluationStatus))
        , ("progress", Just (encodeValue doubleCodec evaluationProgress))
        , ("expectedProgress", Just (encodeValue doubleCodec evaluationExpectedProgress))
        , ("latestValue", encodeValue doubleCodec <$> evaluationLatestValue)
        , ("evaluatedAt", Just (encodeValue timeCodec evaluationEvaluatedAt))
        ]
    decode = withObject "Evaluation" $ \obj ->
      Evaluation
        <$> field goalIdCodec obj "goal"
        <*> field goalStatusCodec obj "status"
        <*> field doubleCodec obj "progress"
        <*> field doubleCodec obj "expectedProgress"
        <*> optionalField doubleCodec obj "latestValue"
        <*> field timeCodec obj "evaluatedAt"

goalStatusCodec :: Codec GoalStatus
goalStatusCodec = Codec encode decode
  where
    encode = \case
      NoData -> String "NoData"
      OnTrack -> String "OnTrack"
      AtRisk -> String "AtRisk"
      OffTrack -> String "OffTrack"
      Achieved -> String "Achieved"
    decode = withText "GoalStatus" $ \tag -> case tag of
      "NoData"   -> pure NoData
      "OnTrack"  -> pure OnTrack
      "AtRisk"   -> pure AtRisk
      "OffTrack" -> pure OffTrack
      "Achieved" -> pure Achieved
      _          -> fail ("Unknown GoalStatus: " <> T.unpack tag)
