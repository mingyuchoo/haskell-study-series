-- | Explicit Identity JSON contract. Keys and tags are stable.
module MyOrg.Serialization.Identity
  ( orgIdCodec
  , userIdCodec
  , goalIdCodec
  , metricIdCodec
  , resourceIdCodec
  , reviewIdCodec
  , moneyCodec
  ) where

import MyOrg.Domain.Identity
import MyOrg.Serialization.Codec

orgIdCodec :: Codec OrgId
orgIdCodec = mapCodec OrgId unOrgId textCodec

userIdCodec :: Codec UserId
userIdCodec = mapCodec UserId unUserId textCodec

goalIdCodec :: Codec GoalId
goalIdCodec = mapCodec GoalId unGoalId textCodec

metricIdCodec :: Codec MetricId
metricIdCodec = mapCodec MetricId unMetricId textCodec

resourceIdCodec :: Codec ResourceId
resourceIdCodec = mapCodec ResourceId unResourceId textCodec

reviewIdCodec :: Codec ReviewId
reviewIdCodec = mapCodec ReviewId unReviewId textCodec

moneyCodec :: Codec Money
moneyCodec = mapCodec Money unMoney integerCodec
