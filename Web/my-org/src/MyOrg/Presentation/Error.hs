module MyOrg.Presentation.Error
  ( describeError
  ) where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import MyOrg.Domain.Error
import MyOrg.Domain.Identity

-- | 사용자에게 보여줄 오류 설명.
describeError :: OrganizationError -> Text
describeError = \case
  NoOrganization -> "조직이 아직 생성되지 않았습니다."
  AmbiguousOrganizations -> "조직이 여러 개입니다. /api/organizations/:id 경로로 대상 조직을 지정하세요."
  OrganizationNotFound oid -> "조직을 찾을 수 없습니다: " <> unOrgId oid
  VersionConflict _ _ -> "확인 중 조직이 변경되었습니다. 최신 상태를 확인하고 다시 시도해주세요."
  OrganizationAlreadyExists -> "조직이 이미 존재합니다."
  GoalNotFound g -> "목표를 찾을 수 없습니다: " <> unGoalId g
  PersonNotFound u -> "구성원을 찾을 수 없습니다: " <> unUserId u
  NoOwner g -> "최종 책임자(Final Owner)가 존재하지 않습니다: " <> unGoalId g
  OwnerMismatch g o a ->
    "목표 "
      <> unGoalId g
      <> "의 책임자("
      <> unUserId o
      <> ")와 권한 소유자("
      <> unUserId a
      <> ")가 다릅니다."
  NoAuthority u -> "권한 기록이 없습니다: " <> unUserId u
  MissingPermissions g u ps ->
    "목표 "
      <> unGoalId g
      <> "의 책임자 "
      <> unUserId u
      <> "에게 다음 권한이 없습니다: "
      <> T.intercalate ", " (map (T.pack . show) (Set.toList ps))
  InsufficientBudget g (Money need) (Money have) ->
    "목표 "
      <> unGoalId g
      <> "에 필요한 예산 "
      <> T.pack (show need)
      <> " 대비 부여된 예산이 "
      <> T.pack (show have)
      <> "입니다."
  InvalidTarget g -> "목표값이 기준값과 같아 성공을 판단할 수 없습니다: " <> unGoalId g
  DeadlineBeforeStart g -> "마감이 시작일보다 앞섭니다: " <> unGoalId g
  GoalAlreadyActive g -> "이미 활성화된 목표입니다: " <> unGoalId g
  GoalNotActive g -> "활성화되지 않은 목표입니다: " <> unGoalId g
  ParentGoalNotFound g p ->
    "목표 " <> unGoalId g <> "의 상위 목표를 찾을 수 없습니다: " <> unGoalId p
  StorageFailure -> "저장에 실패했습니다. 상태를 변경하지 않았습니다."
  InvalidInput t -> t
  DuplicateId t -> "이미 사용 중인 식별자입니다: " <> t
