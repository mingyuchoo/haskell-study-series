-- | Explicit Goal JSON contract. Keys and tags are stable.
module MyOrg.Serialization.Goal
  ( metricDirectionCodec
  , metricCodec
  , goalCodec
  ) where

import Data.Aeson (Value (String), withObject, withText)
import Data.Text qualified as T
import MyOrg.Domain.Goal.Types
import MyOrg.Serialization.Authority
import MyOrg.Serialization.Codec
import MyOrg.Serialization.Identity

metricCodec :: Codec Metric
metricCodec = Codec encode decode
  where
    encode Metric {..} =
      record
        [ ("id", Just (encodeValue metricIdCodec metricId))
        , ("name", Just (encodeValue textCodec metricName))
        , ("unit", Just (encodeValue textCodec metricUnit))
        , ("direction", Just (encodeValue metricDirectionCodec metricDirection))
        ]
    decode = withObject "Metric" $ \obj ->
      Metric
        <$> field metricIdCodec obj "id"
        <*> field textCodec obj "name"
        <*> field textCodec obj "unit"
        <*> field metricDirectionCodec obj "direction"

goalCodec :: Codec Goal
goalCodec = Codec encode decode
  where
    encode Goal {..} =
      record
        [ ("id", Just (encodeValue goalIdCodec goalId))
        , ("organization", Just (encodeValue orgIdCodec goalOrganization))
        , ("description", Just (encodeValue textCodec goalDescription))
        , ("metric", Just (encodeValue metricCodec goalMetric))
        , ("baseline", Just (encodeValue doubleCodec goalBaseline))
        , ("target", Just (encodeValue doubleCodec goalTarget))
        , ("startsAt", Just (encodeValue timeCodec goalStartsAt))
        , ("deadline", Just (encodeValue timeCodec goalDeadline))
        , ("parent", encodeValue goalIdCodec <$> goalParent)
        ,
          ( "requiredPermissions"
          , Just (encodeValue (setCodec permissionCodec) goalRequiredPermissions)
          )
        , ("requiredBudget", Just (encodeValue moneyCodec goalRequiredBudget))
        ]
    decode = withObject "Goal" $ \obj ->
      Goal
        <$> field goalIdCodec obj "id"
        <*> field orgIdCodec obj "organization"
        <*> field textCodec obj "description"
        <*> field metricCodec obj "metric"
        <*> field doubleCodec obj "baseline"
        <*> field doubleCodec obj "target"
        <*> field timeCodec obj "startsAt"
        <*> field timeCodec obj "deadline"
        <*> optionalField goalIdCodec obj "parent"
        <*> field (setCodec permissionCodec) obj "requiredPermissions"
        <*> field moneyCodec obj "requiredBudget"

metricDirectionCodec :: Codec MetricDirection
metricDirectionCodec = Codec encode decode
  where
    encode = \case
      HigherIsBetter -> String "HigherIsBetter"
      LowerIsBetter -> String "LowerIsBetter"
    decode = withText "MetricDirection" $ \tag -> case tag of
      "HigherIsBetter" -> pure HigherIsBetter
      "LowerIsBetter"  -> pure LowerIsBetter
      _                -> fail ("Unknown MetricDirection: " <> T.unpack tag)
