-- | 목표의 생애주기: 초안(Draft)에서 활성(Active)으로.
--
-- 활성 목표는 반드시 책임자와 충분한 권한과 지표를 함께 가진다.
-- 이 불변식은 'ActiveGoal' 생성자를 감추고 'activate'만 노출하여 강제한다.
module MyOrg.Domain.Goal
  ( DraftGoal (..)
  , ActiveGoal
  , activeGoal
  , activeOwnership
  , activeAuthority
  , activeMetric
  , activate
  , validateGoal
  , validateDraft
  , missingPermissions
  , authorityCoverage
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import MyOrg.Domain.Identity
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Authority
import MyOrg.Domain.Error

-- | 아직 활성화되지 않은 목표.
newtype DraftGoal = DraftGoal {unDraftGoal :: Goal}
  deriving stock (Show, Eq)

-- | 활성화된 목표. 생성자는 외부에 노출하지 않는다.
data ActiveGoal = ActiveGoal
  { internalGoal :: Goal
  , internalOwnership :: Ownership
  , internalAuthority :: Authority
  , internalMetric :: Metric
  }
  deriving stock (Show, Eq)

-- | 초안 자체의 정합성. 책임자와 무관하게 성립해야 하는 규칙.
validateDraft :: Goal -> Either OrganizationError ()
validateDraft g
  | any (T.null . T.strip) [goalDescription g, metricName (goalMetric g), metricUnit (goalMetric g), unMetricId (metricId (goalMetric g))] = Left (InvalidInput "목표 설명과 KPI 이름·단위·식별자가 필요합니다.")
  | any (\x -> isNaN x || isInfinite x) [goalBaseline g, goalTarget g] = Left (InvalidInput "지표는 유한한 숫자여야 합니다.")
  | goalRequiredBudget g < 0 = Left (InvalidInput "예산은 음수일 수 없습니다.")
  | metricDirection (goalMetric g) == HigherIsBetter && goalTarget g < goalBaseline g = Left (InvalidInput "증가 지표의 목표값은 기준값보다 커야 합니다.")
  | metricDirection (goalMetric g) == LowerIsBetter && goalTarget g > goalBaseline g = Left (InvalidInput "감소 지표의 목표값은 기준값보다 작아야 합니다.")
  | goalTarget g == goalBaseline g = Left (InvalidTarget (goalId g))
  | goalDeadline g <= goalStartsAt g = Left (DeadlineBeforeStart (goalId g))
  | otherwise = Right ()

-- | 책임에 비해 부족한 권한 집합.
missingPermissions :: Goal -> Authority -> Set Permission
missingPermissions g a = goalRequiredPermissions g `Set.difference` grantedPermissions a

-- | 책임자가 목표 달성에 필요한 자원 중 실제로 통제하는 비율(0..1).
--
-- 필요한 권한 각각을 1점, 예산 요건을 1점으로 계산한다.
-- 필요한 자원이 전혀 없으면 1을 반환한다.
authorityCoverage :: Goal -> Authority -> Double
authorityCoverage g a
  | total == 0 = 1
  | otherwise = fromIntegral controlled / fromIntegral total
 where
  perms = goalRequiredPermissions g
  needsBudget = goalRequiredBudget g > Money 0
  total = Set.size perms + (if needsBudget then 1 else 0) :: Int
  controlledPerms = Set.size (perms `Set.intersection` grantedPermissions a)
  budgetOk = needsBudget && authorityBudgetLimit a >= goalRequiredBudget g
  controlled = controlledPerms + (if budgetOk then 1 else 0)

-- | 목표, 책임, 권한을 합쳐 검증한다.
--
-- 책임자나 권한이 'Nothing'이면 실패한다. 즉,
--
-- > Goal -> Maybe Owner
--
-- 를 허용하는 대신, 검증을 통과한 값만 'ActiveGoal'이 된다.
validateGoal
  :: Goal
  -> Maybe Ownership
  -> Maybe Authority
  -> Either OrganizationError ActiveGoal
validateGoal g mo ma = do
  validateDraft g
  o <- maybe (Left (NoOwner (goalId g))) Right mo
  a <- maybe (Left (NoAuthority (ownershipOwner o))) Right ma
  activate (DraftGoal g) o a

-- | 초안을 활성 목표로 전환한다.
--
-- 실패 조건:
--
-- * 책임자가 가리키는 목표가 이 목표가 아님
-- * 권한 기록의 소유자가 책임자가 아님
-- * 필요한 권한 중 일부가 없음
-- * 필요한 예산보다 부여된 예산이 작음
activate :: DraftGoal -> Ownership -> Authority -> Either OrganizationError ActiveGoal
activate (DraftGoal g) o a = do
  validateDraft g
  let gid = goalId g
      owner = ownershipOwner o
  if ownershipGoal o /= gid
    then Left (NoOwner gid)
    else Right ()
  if authorityOwner a /= owner
    then Left (OwnerMismatch gid owner (authorityOwner a))
    else Right ()
  let missing = missingPermissions g a
  if Set.null missing
    then Right ()
    else Left (MissingPermissions gid owner missing)
  if authorityBudgetLimit a >= goalRequiredBudget g
    then Right ()
    else Left (InsufficientBudget gid (goalRequiredBudget g) (authorityBudgetLimit a))
  pure
    ActiveGoal
      { internalGoal = g
      , internalOwnership = o
      , internalAuthority = a
      , internalMetric = goalMetric g
      }

activeGoal :: ActiveGoal -> Goal
activeGoal = internalGoal
activeOwnership :: ActiveGoal -> Ownership
activeOwnership = internalOwnership
activeAuthority :: ActiveGoal -> Authority
activeAuthority = internalAuthority
activeMetric :: ActiveGoal -> Metric
activeMetric = internalMetric
