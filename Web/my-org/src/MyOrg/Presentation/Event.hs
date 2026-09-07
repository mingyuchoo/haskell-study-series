module MyOrg.Presentation.Event
  ( describeEvent
  ) where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import MyOrg.Domain.Authority
import MyOrg.Domain.Event.Types
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Identity
import MyOrg.Domain.Organization
import MyOrg.Domain.Result
import MyOrg.Domain.Review.Types

-- | 타임라인에 표시할 한 줄 설명.
describeEvent :: OrganizationEvent -> Text
describeEvent = \case
  OrganizationScoped _ event -> describeEvent event
  OrganizationRenamed oid name -> "조직 이름 변경: " <> unOrgId oid <> " → " <> name
  OrganizationDeleted oid -> "조직 논리 삭제: " <> unOrgId oid
  DiscoverySaved _ -> "조직 현황·업무 흐름과 에이전트 초안 검토 저장"
  DemoSeeded _ -> "체험용 데모 시드 생성 완료"
  OrganizationCreated o -> "조직 생성: " <> organizationName o
  PersonAdded p -> "구성원 추가: " <> personName p <> " (" <> personRole p <> ")"
  EmployeeAdded p _ -> "구성원 추가: " <> personName p
  PersonUpdated p _ -> "구성원 정보 수정: " <> personName p
  PersonDeactivated uid successor -> "구성원 비활성화: " <> unUserId uid <> maybe "" ((" → 인계: " <>) . unUserId) successor
  GoalCreated g -> "목표 생성: " <> goalDescription g
  OwnerAssigned g u -> "책임자 지정: " <> unGoalId g <> " -> " <> unUserId u
  AuthorityGranted u a ->
    "권한 부여: "
      <> unUserId u
      <> " = "
      <> T.intercalate ", " (map (T.pack . show) (Set.toList (grantedPermissions a)))
      <> ", 예산 "
      <> T.pack (show (unMoney (authorityBudgetLimit a)))
  AuthorityRevoked u p -> "권한 회수: " <> unUserId u <> " - " <> T.pack (show p)
  GoalActivated g -> "목표 활성화: " <> unGoalId g
  ResultReported g r ->
    "결과 보고: " <> unGoalId g <> " = " <> T.pack (show (resultValue r))
  GoalEvaluated g e ->
    "평가: " <> unGoalId g <> " = " <> T.pack (show (evaluationStatus e))
  ReviewHeld r ->
    "리뷰: "
      <> unGoalId (reviewGoal r)
      <> ", 결정 "
      <> T.pack (show (length (reviewDecisions r)))
      <> "건, 학습 "
      <> T.pack (show (length (reviewLearnings r)))
      <> "건"
  StrategyChanged g t -> "전략 변경: " <> unGoalId g <> " - " <> t
