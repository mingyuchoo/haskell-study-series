module MyOrg.Presentation.Review
  ( describeReviewWarning
  ) where

import Data.Text (Text)
import MyOrg.Domain.Review

describeReviewWarning :: ReviewWarning -> Text
describeReviewWarning = \case
  NoDecisionProduced -> "This review produced no decision."
  DecisionWithoutDeadline t -> "결정에 기한이 없습니다: " <> t
