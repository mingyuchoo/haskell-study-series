-- | Pure Result domain concepts.
module MyOrg.Domain.Result (Result(..), GoalStatus(..), Evaluation(..)) where

import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import MyOrg.Domain.Identity

-- | 지표의 실측값 보고.
data Result = Result
  { resultGoal :: GoalId
  , resultValue :: Double
  , resultReportedAt :: UTCTime
  , resultReportedBy :: UserId
  , resultNote :: Text
  }
  deriving stock (Show, Eq, Generic)



data GoalStatus
  = NoData
  -- ^ 보고된 결과가 없음
  | OnTrack
  -- ^ 정상
  | AtRisk
  -- ^ 위험
  | OffTrack
  -- ^ 이탈
  | Achieved
  -- ^ 달성
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

data Evaluation = Evaluation
  { evaluationGoal :: GoalId
  , evaluationStatus :: GoalStatus
  , evaluationProgress :: Double
  -- ^ 0 = baseline, 1 = target. 실제 달성 비율
  , evaluationExpectedProgress :: Double
  -- ^ 시간 경과 기준으로 기대되는 비율
  , evaluationLatestValue :: Maybe Double
  , evaluationEvaluatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)
