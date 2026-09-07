-- | Deterministic fictional examples. Events are honestly stamped at import time;
-- successive result samples illustrate a trend, not fabricated past audit dates.
module MyOrg.Demo
  ( demoOrganizationId
  , demoCommands
  , demoCoreCommands
  , demoDiscovery
  , demoEvents
  , legacyDemoEvents
  , isDemoEpoch
  , isDemoStore
  ) where

import Control.Monad (foldM)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime, addUTCTime)
import MyOrg.Application
import MyOrg.Domain.Authority
import MyOrg.Domain.Discovery
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

-- | The full seed: the frozen operating scenario plus the surveyed workflows
-- that feed the agent design pages. The survey is appended last so the
-- original 51-event scenario keeps its historical shape.
demoCommands :: UTCTime -> [Command]
demoCommands now = core <> [SaveDiscovery demoDiscovery (length core)]
  where
    core = demoCoreCommands now

-- | 현황 조사 시드. 확인된 사실, 미확인, 개선안과 참조 필드를 모두 담아
-- 규칙 기반 도출과 진단을 체험할 수 있게 한다. 기준일은 미확인으로 둔다.
demoDiscovery :: Discovery
demoDiscovery =
  Discovery
    "북극성 스튜디오 고객 성공과 영업 운영 현황 (체험용 가상 자료)"
    ""
    [ Observation
        "demo-o-refund"
        "환불 승인 권한"
        "환불은 고객 성공 책임자가 검토하고 CEO가 최종 승인한다."
        Confirmed
        "체험용 가상 운영 절차 문서 3항"
    , Observation
        "demo-o-legal"
        "엔터프라이즈 계약 검토 담당"
        "법무 검토를 누가 맡는지 확인되지 않았다."
        Unknown
        "파트너십 책임자 한유진에게 확인 예정"
    , Observation
        "demo-o-onboarding"
        "온보딩 안내 자동화"
        "온보딩 이메일 초안을 자동으로 만들고 사람이 검토한 뒤 발송한다."
        Proposed
        ""
    ]
    [ (emptyWorkflow "demo-w-inquiry" "고객 문의 분류와 답변 초안")
        { workflowRole = "고객 성공 담당"
        , workflowRolePerson = Just success
        , workflowTrigger = "새 문의 접수"
        , workflowInputs = "문의 내용, 고객 계약 정보"
        , workflowTools = "CRM, 고객지원 문서"
        , workflowOutputs = "문의 분류, 답변 초안"
        , workflowHandoff = "환불 문의는 환불 검토와 집행으로 전달"
        , workflowHandoffWorkflows = ["demo-w-refund"]
        , workflowStatus = Confirmed
        , workflowEvidence = "체험용 가상 인터뷰 메모 (정하린)"
        }
    , (emptyWorkflow "demo-w-refund" "환불 검토와 집행")
        { workflowRole = "고객 성공 책임자"
        , workflowRolePerson = Just success
        , workflowTrigger = "환불 문의 인계"
        , workflowInputs = "환불 사유, 결제 내역"
        , workflowTools = "결제 관리자 콘솔"
        , workflowOutputs = "환불 승인 요청, 환불 처리 기록"
        , workflowApproval = "집행 전 CEO 승인"
        , workflowApprovalPerson = Just ceo
        , workflowStatus = Confirmed
        , workflowEvidence = "체험용 가상 운영 절차 문서 3항"
        }
    , (emptyWorkflow "demo-w-proposal" "엔터프라이즈 제안서 작성")
        { workflowRole = "영업 담당"
        , workflowRolePerson = Just sales
        , workflowTrigger = "영업 기회 등록"
        , workflowInputs = "고객 요구사항, 가격표"
        , workflowTools = "CRM, 제안서 템플릿"
        , workflowOutputs = "제안서 초안, 견적"
        , workflowHandoff = "계약 조건은 파트너십 계약 검토로 전달"
        , workflowHandoffWorkflows = ["demo-w-contract"]
        , workflowApproval = "할인율 10% 초과 시 가격 결정 권한자 승인"
        , workflowApprovalPermission = Just Pricing
        , workflowStatus = Confirmed
        , workflowEvidence = "체험용 가상 영업 플레이북"
        }
    , (emptyWorkflow "demo-w-contract" "파트너십 계약 검토")
        { workflowTrigger = "계약 초안 접수"
        , workflowInputs = "계약 초안"
        , workflowOutputs = "검토 의견"
        , workflowStatus = Unknown
        , workflowEvidence = "법무 검토 절차 미확인. 한유진에게 확인 예정"
        }
    ]
    (DiscoveryReview Pending "")
  where
    ceo = UserId "demo-ceo"
    sales = UserId "demo-sales"
    success = UserId "demo-success"

-- | The original operating scenario. Its event shape is frozen because
-- historical demo stores are recognized by matching this exact prefix.
demoCoreCommands :: UTCTime -> [Command]
demoCoreCommands now =
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
    <> [ ActivateGoal (gid g)
       | g <- ["company", "revenue", "customers", "reliability", "retention"]
       ]
    <> concat
      [ samples "revenue" sales [34, 38, 42]
      , samples "customers" sales [1300, 1800, 2280]
      , samples "reliability" infra [10, 9, 9]
      , samples "retention" success [7, 5, 3]
      ]
    <> [ EvaluateGoal (gid g)
       | g <- ["company", "revenue", "customers", "reliability", "retention"]
       ]
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
demoEvents = seedEvents demoCommands

-- | The historical 52-event seed (51 scenario events plus the marker) that
-- existing demo stores and the fixed fixtures contain.
legacyDemoEvents :: UTCTime -> Either OrganizationError [StoredEvent]
legacyDemoEvents = seedEvents demoCoreCommands

seedEvents :: (UTCTime -> [Command]) -> UTCTime -> Either OrganizationError [StoredEvent]
seedEvents commands now = do
  (_, events) <- foldM step (emptyState, []) (commands now)
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
legacySeed events@(first : _) = case legacyDemoEvents (storedAt first) of
  Left _ -> False
  Right expected ->
    let original = filter (\event -> case storedEvent event of DemoSeeded _ -> False; _ -> True) expected
     in length events >= length original
          && map storedEvent (take (length original) events) == map storedEvent original
