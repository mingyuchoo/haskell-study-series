-- | Pure Goal.Types domain concepts.
module MyOrg.Domain.Goal.Types
  ( MetricDirection (..)
  , Metric (..)
  , Goal (..)
  ) where

import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import MyOrg.Domain.Authority (Permission)
import MyOrg.Domain.Identity

-- | 지표의 방향. 매출은 높을수록 좋고, 이탈률은 낮을수록 좋다.
data MetricDirection = HigherIsBetter | LowerIsBetter
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

data Metric = Metric
  { metricId        :: MetricId
  , metricName      :: Text
  , metricUnit      :: Text
  , metricDirection :: MetricDirection
  }
  deriving stock (Show, Eq, Generic)

-- | 목표 정의. 이 자체로는 초안(Draft)이며, 책임자와 권한이 붙어야 활성화된다.
data Goal = Goal
  { goalId                  :: GoalId
  , goalOrganization        :: OrgId
  , goalDescription         :: Text
  , goalMetric              :: Metric
  , goalBaseline            :: Double
    -- ^ 시작 시점의 지표 값
  , goalTarget              :: Double
    -- ^ 마감 시점에 도달해야 하는 값
  , goalStartsAt            :: UTCTime
  , goalDeadline            :: UTCTime
  , goalParent              :: Maybe GoalId
    -- ^ 상위 목표. 상위 목표는 하위 목표에 의존한다(DependsOn).
  , goalRequiredPermissions :: Set Permission
    -- ^ 이 목표를 달성하기 위해 책임자가 가져야 하는 결정 권한
  , goalRequiredBudget      :: Money
    -- ^ 이 목표를 달성하기 위해 책임자가 집행할 수 있어야 하는 예산
  }
  deriving stock (Show, Eq, Generic)
