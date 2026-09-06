-- | 결과를 목표에 비추어 평가한다.
module MyOrg.Domain.Evaluation
  ( evaluateGoal
  , progressOf
  , expectedProgress
  , classify
  , latestResult
  ) where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Time (UTCTime, diffUTCTime)
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Result

-- | 가장 최근에 보고된 결과.
latestResult :: [Result] -> Maybe Result
latestResult rs = case sortOn (Down . resultReportedAt) rs of
  [] -> Nothing
  (r : _) -> Just r

-- | 기준값을 0, 목표값을 1로 두었을 때 현재 값의 위치. 0 이상 1 이하로 자른다.
--
-- 방향(높을수록 좋음, 낮을수록 좋음)은 기준값과 목표값의 순서에 이미 반영되어 있다.
progressOf :: Goal -> Double -> Double
progressOf g value
  | span' == 0 = if value == goalTarget g then 1 else 0
  | otherwise = clamp ((value - goalBaseline g) / span')
 where
  span' = goalTarget g - goalBaseline g

-- | 시간 경과에 따라 기대되는 진척률. 0 이상 1 이하.
expectedProgress :: UTCTime -> Goal -> Double
expectedProgress now g
  | total <= 0 = 1
  | otherwise = clamp (elapsed / total)
 where
  total = realToFrac (diffUTCTime (goalDeadline g) (goalStartsAt g)) :: Double
  elapsed = realToFrac (diffUTCTime now (goalStartsAt g)) :: Double

clamp :: Double -> Double
clamp = max 0 . min 1

-- | 진척률과 기대 진척률로 상태를 결정한다.
--
-- * 진척률이 1 이상이면 달성
-- * 기대치 대비 10%p 이내면 정상
-- * 기대치 대비 30%p 이내면 위험
-- * 그 밖에는 이탈
classify :: Double -> Double -> GoalStatus
classify progress expected
  | progress >= 1 = Achieved
  | progress >= expected - 0.10 = OnTrack
  | progress >= expected - 0.30 = AtRisk
  | otherwise = OffTrack

evaluateGoal :: UTCTime -> Goal -> [Result] -> Evaluation
evaluateGoal now g rs =
  Evaluation
    { evaluationGoal = goalId g
    , evaluationStatus = status
    , evaluationProgress = progress
    , evaluationExpectedProgress = expected
    , evaluationLatestValue = resultValue <$> latest
    , evaluationEvaluatedAt = now
    }
 where
  latest = latestResult rs
  expected = expectedProgress now g
  progress = maybe 0 (progressOf g . resultValue) latest
  status = case latest of
    Nothing -> NoData
    Just _ -> classify progress expected
