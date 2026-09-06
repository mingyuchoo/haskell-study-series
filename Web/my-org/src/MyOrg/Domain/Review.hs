-- | 리뷰 회의의 규칙.
--
-- 회의의 출력은 반드시 결정(Decision), 학습(Learning) 중 하나 이상이어야 한다.
module MyOrg.Domain.Review
  ( ReviewWarning (..)
  , checkReview
  , describeReviewWarning
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import MyOrg.Domain.Review.Types

data ReviewWarning = NoDecisionProduced
                   | DecisionWithoutDeadline Text
  deriving stock (Show, Eq, Generic)

-- | 리뷰가 조직에 무엇을 남겼는지 검사한다.
checkReview :: Review -> [ReviewWarning]
checkReview r =
  [NoDecisionProduced | null (reviewDecisions r) && null (reviewLearnings r)]
    ++ [ DecisionWithoutDeadline (decisionText d)
       | d <- reviewDecisions r
       , decisionDeadline d == Nothing
       ]

describeReviewWarning :: ReviewWarning -> Text
describeReviewWarning = \case
  NoDecisionProduced -> "This review produced no decision."
  DecisionWithoutDeadline t -> "결정에 기한이 없습니다: " <> t
