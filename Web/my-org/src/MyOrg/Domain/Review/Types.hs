-- | Pure Review.Types domain concepts.
module MyOrg.Domain.Review.Types
  ( Learning (..)
  , Decision (..)
  , Review (..)
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import MyOrg.Domain.Identity
import MyOrg.Domain.Result

newtype Learning = Learning { learningText :: Text }
  deriving stock (Show, Eq, Generic)

-- | 회의의 출력 중 하나. 결정에는 반드시 담당자가 있다.
data Decision = Decision
  { decisionText     :: Text
  , decisionOwner    :: UserId
  , decisionDeadline :: Maybe UTCTime
  }
  deriving stock (Show, Eq, Generic)

-- | 리뷰 회의는 캘린더 이벤트가 아니라 데이터 구조다.
data Review = Review
  { reviewId         :: ReviewId
  , reviewGoal       :: GoalId
  , reviewResult     :: Maybe Result
  , reviewEvaluation :: Evaluation
  , reviewLearnings  :: [Learning]
  , reviewDecisions  :: [Decision]
  , reviewHeldAt     :: UTCTime
  , reviewNote       :: Text
  }
  deriving stock (Show, Eq, Generic)
