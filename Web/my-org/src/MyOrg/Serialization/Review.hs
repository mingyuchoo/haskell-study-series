-- | Explicit Review JSON contract. Keys and tags are stable.
module MyOrg.Serialization.Review
  ( learningCodec
  , decisionCodec
  , reviewCodec
  ) where

import Data.Aeson (withObject)
import MyOrg.Domain.Review.Types
import MyOrg.Serialization.Codec
import MyOrg.Serialization.Identity
import MyOrg.Serialization.Result

learningCodec :: Codec Learning
learningCodec = Codec encode decode
  where
    encode Learning {..} =
      record
        [ ("text", Just (encodeValue textCodec learningText))
        ]
    decode = withObject "Learning" $ \obj ->
      Learning
        <$> field textCodec obj "text"

decisionCodec :: Codec Decision
decisionCodec = Codec encode decode
  where
    encode Decision {..} =
      record
        [ ("text", Just (encodeValue textCodec decisionText))
        , ("owner", Just (encodeValue userIdCodec decisionOwner))
        , ("deadline", encodeValue timeCodec <$> decisionDeadline)
        ]
    decode = withObject "Decision" $ \obj ->
      Decision
        <$> field textCodec obj "text"
        <*> field userIdCodec obj "owner"
        <*> optionalField timeCodec obj "deadline"

reviewCodec :: Codec Review
reviewCodec = Codec encode decode
  where
    encode Review {..} =
      record
        [ ("id", Just (encodeValue reviewIdCodec reviewId))
        , ("goal", Just (encodeValue goalIdCodec reviewGoal))
        , ("result", encodeValue resultCodec <$> reviewResult)
        , ("evaluation", Just (encodeValue evaluationCodec reviewEvaluation))
        , ("learnings", Just (encodeValue (listCodec learningCodec) reviewLearnings))
        , ("decisions", Just (encodeValue (listCodec decisionCodec) reviewDecisions))
        , ("heldAt", Just (encodeValue timeCodec reviewHeldAt))
        , ("note", Just (encodeValue textCodec reviewNote))
        ]
    decode = withObject "Review" $ \obj ->
      Review
        <$> field reviewIdCodec obj "id"
        <*> field goalIdCodec obj "goal"
        <*> optionalField resultCodec obj "result"
        <*> field evaluationCodec obj "evaluation"
        <*> field (listCodec learningCodec) obj "learnings"
        <*> field (listCodec decisionCodec) obj "decisions"
        <*> field timeCodec obj "heldAt"
        <*> field textCodec obj "note"
