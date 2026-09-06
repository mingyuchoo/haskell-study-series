-- | Pure Identity domain concepts.
module MyOrg.Domain.Identity (OrgId(..), UserId(..), GoalId(..), MetricId(..), ResourceId(..), ReviewId(..), Money(..)) where

import Data.Text (Text)
import GHC.Generics (Generic)

newtype OrgId = OrgId {unOrgId :: Text}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

newtype UserId = UserId {unUserId :: Text}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

newtype GoalId = GoalId {unGoalId :: Text}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

newtype MetricId = MetricId {unMetricId :: Text}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

newtype ResourceId = ResourceId {unResourceId :: Text}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

newtype ReviewId = ReviewId {unReviewId :: Text}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

-- | 금액. 통화 단위는 조직 단위로 고정한다(기본 KRW).
newtype Money = Money {unMoney :: Integer}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Num)
