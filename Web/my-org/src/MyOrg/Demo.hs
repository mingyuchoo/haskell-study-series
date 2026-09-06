-- | Deterministic fictional examples. Events are honestly stamped at import time;
-- successive result samples illustrate a trend, not fabricated past audit dates.
module MyOrg.Demo
  ( demoOrganizationId
  , demoCommands
  , demoEvents
  , isDemoEpoch
  , isDemoStore
  ) where

import Control.Monad (foldM)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime, addUTCTime)
import MyOrg.Application
import MyOrg.Domain.Authority
import MyOrg.Domain.Error
import MyOrg.Domain.Event.Types
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Identity
import MyOrg.Domain.Organization
import MyOrg.Domain.Reducer
import MyOrg.Domain.Review.Types
import MyOrg.Domain.State

demoOrganizationId :: OrgId
demoOrganizationId = OrgId "demo-northstar-v2"

demoCommands :: UTCTime -> [Command]
demoCommands now =
  [ CreateOrganization demoOrganizationId "북극성 스튜디오 · 체험 조직"
  , AddPerson (Person ceo "박서준" "CEO" Nothing)
  , AddPerson (Person sales "김민서" "영업·그로스 책임자" (Just ceo))
  , AddPerson (Person productLead "이지원" "제품 책임자" (Just ceo))
  , AddPerson (Person infra "최도윤" "인프라 책임자" (Just ceo))
  , AddPerson (Person success "정하린" "고객 성공 책임자" (Just ceo))
  , AddPerson (Person partner "한유진" "파트너십 책임자" (Just ceo))
  , GrantAuthority (authority ceo 500000000 (Set.toList allPermissions))
  , GrantAuthority (authority sales 0 [Pricing, Marketing])
  , GrantAuthority (authority productLead 10000000 [ProductLaunch])
  , GrantAuthority (authority infra 0 [Infrastructure])
  , GrantAuthority (authority success 0 [])
  , GrantAuthority (authority partner 0 [Contracting])
  , CreateGoal
      ( goal
          "company"
          "지속 가능한 전사 성장"
          "growth-index"
          "전사 성장 지수"
          "점"
          HigherIsBetter
          0
          100
          Nothing
          [BudgetApproval]
          1000000
      )
  , CreateGoal
      ( child
          "revenue"
          "엔터프라이즈 매출 확대"
          "revenue"
          "엔터프라이즈 매출"
          "억원"
          HigherIsBetter
          30
          50
          [Pricing, Marketing]
          0
      )
  , CreateGoal
      ( child
          "customers"
          "새로운 고객과의 첫 만남"
          "customers"
          "신규 고객 수"
          "명"
          HigherIsBetter
          1000
          5000
          [Marketing]
          0
      )
  , CreateGoal
      ( child
          "reliability"
          "고객이 믿고 쓰는 안정적인 서비스"
          "incidents"
          "월간 장애 건수"
          "건"
          LowerIsBetter
          10
          0
          [Infrastructure]
          0
      )
  , CreateGoal (child "retention" "고객이 계속 찾는 서비스" "churn" "고객 이탈률" "%" LowerIsBetter 8 3 [] 0)
  , CreateGoal
      ( child
          "launch"
          "신제품 출시로 고객 가치 전달"
          "launch-progress"
          "제품 출시 준비율"
          "%"
          HigherIsBetter
          0
          100
          [ProductLaunch, Hiring]
          30000000
      )
  , CreateGoal
      ( child
          "partners"
          "새로운 파트너십 확장"
          "partnerships"
          "신규 파트너 계약"
          "건"
          HigherIsBetter
          0
          10
          [Contracting]
          0
      )
  ]
    <> [ AssignOwner (gid g) u
       | (g, u) <-
           [ ("company", ceo)
           , ("revenue", sales)
           , ("customers", sales)
           , ("reliability", infra)
           , ("retention", success)
           , ("launch", productLead)
           ]
       ]
    <> [ActivateGoal (gid g) | g <- ["company", "revenue", "customers", "reliability", "retention"]]
    <> concat
      [ samples "revenue" sales [34, 38, 42]
      , samples "customers" sales [1300, 1800, 2280]
      , samples "reliability" infra [10, 9, 9]
      , samples "retention" success [7, 5, 3]
      ]
    <> [EvaluateGoal (gid g) | g <- ["company", "revenue", "customers", "reliability", "retention"]]
    <> [ HoldReview
           (ReviewId "demo-review-retention")
           (gid "retention")
           [Learning "초기 2주 고객 온보딩 개선이 이탈률 감소로 이어졌다."]
           [Decision "온보딩 실험을 엔터프라이즈 고객으로 확대" success (Just (addUTCTime (14 * 86400) now))]
           "체험용 좋은 회고: 결과, 학습, 결정 담당자와 기한을 함께 남겼습니다."
       , HoldReview
           (ReviewId "demo-review-customers")
           (gid "customers")
           []
           []
           "체험용 빈 회고: 현황 공유로 끝나 결정과 학습이 없습니다. O040 경고를 확인하세요."
       , ChangeStrategy (gid "revenue") "체험용 전략: 범용 할인에서 고객별 유료 파일럿 제안으로 전환한다."
       ]
  where
    ceo = UserId "demo-ceo"
    sales = UserId "demo-sales"
    productLead = UserId "demo-product"
    infra = UserId "demo-infra"
    success = UserId "demo-success"
    partner = UserId "demo-partner"
    gid = GoalId . ("demo-" <>)
    authority owner budget ps = Authority owner budget False False (Set.fromList ps)
    goal ident description mid metricName unit direction baseline target parent ps budget =
      Goal
        (gid ident)
        demoOrganizationId
        description
        (Metric (MetricId ("demo-" <> mid)) metricName unit direction)
        baseline
        target
        (addUTCTime (-50 * 86400) now)
        (addUTCTime (50 * 86400) now)
        parent
        (Set.fromList ps)
        budget
    child ident description mid metricName unit direction baseline target =
      goal
        ident
        description
        mid
        metricName
        unit
        direction
        baseline
        target
        (Just (gid "company"))
    samples ident reporter values =
      [ ReportResult (gid ident) value reporter "체험용 연속 측정 샘플입니다. 실제 과거 측정이 아니며 가져온 시각으로 기록했습니다."
      | value <- values
      ]

demoEvents :: UTCTime -> Either OrganizationError [StoredEvent]
demoEvents now = do
  (_, events) <- foldM step (emptyState, []) (demoCommands now)
  pure
    ( events
        <> [ StoredEvent
               (length events + 1)
               now
               (Just (UserId "demo-ceo"))
               (DemoSeeded demoOrganizationId)
           ]
    )
  where
    step (st, events) command = do
      changes <- executeCommand now st command
      let actor =
            if Map.member (UserId "demo-ceo") (statePeople st)
              then Just (UserId "demo-ceo")
              else Nothing
          additions = zipWith (\n change -> StoredEvent n now actor change) [length events + 1 ..] changes
      pure (applyEvents st additions, events <> additions)

-- | A marker is written only by the atomic seed operation. For the original v2
-- format, recognize the entire deterministic 51-event seed prefix, not its ID.
isDemoEpoch :: [StoredEvent] -> Bool
isDemoEpoch events = let epoch = currentEpoch events in marked epoch || legacySeed epoch

isDemoStore :: [StoredEvent] -> Bool
isDemoStore events = marked events || legacySeed events

marked :: [StoredEvent] -> Bool
marked =
  any
    ( \event -> case storedEvent event of DemoSeeded oid -> oid == demoOrganizationId; _ -> False
    )

legacySeed :: [StoredEvent] -> Bool
legacySeed [] = False
legacySeed events@(first : _) = case demoEvents (storedAt first) of
  Left _ -> False
  Right expected ->
    let original = filter (\event -> case storedEvent event of DemoSeeded _ -> False; _ -> True) expected
     in length events >= length original
          && map storedEvent (take (length original) events) == map storedEvent original
