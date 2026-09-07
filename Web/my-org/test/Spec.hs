module Main
  ( main
  ) where

import ApiSmokeSpec qualified
import ContractSpec qualified
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, bracket, try)
import Control.Monad (foldM, forM, when)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Either (isLeft, isRight)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (UTCTime, addUTCTime)
import DeleteSmokeSpec qualified
import DemoSmokeSpec qualified
import DiscoverySmokeSpec qualified
import EmployeeSmokeSpec qualified
import EmployeeSpec qualified
import Lib qualified
import MyOrg.Application
import MyOrg.Demo
import MyOrg.Domain.Analysis qualified as Analysis
import MyOrg.Domain.Compiler
import MyOrg.Domain.Evaluation
import MyOrg.Domain.Event
import MyOrg.Domain.Goal
import MyOrg.Domain.Graph
import MyOrg.Domain.Review
import MyOrg.Presentation.Analysis qualified as AnalysisView
import MyOrg.Presentation.Diagnostic qualified as DiagnosticView
import MyOrg.Presentation.Review (describeReviewWarning)
import MyOrg.Registry
import MyOrg.Serialization.JSON (eitherDecodeWire, encodeWire)
import MyOrg.Store
import MyOrg.Types
import OrganizationsSmokeSpec qualified
import PlanSpec qualified
import QuerySpec qualified
import RegistrySpec (registrySpec)
import SQLiteStoreSpec qualified
import StartupSmokeSpec qualified
import System.Directory
import System.Environment (getArgs)
import System.IO (BufferMode (LineBuffering), hClose, hSetBuffering, openTempFile, stdout)
import Test.Hspec
import Test.QuickCheck (property)
import WireSpec qualified

main :: IO ()
main = do
  args <- getArgs
  if args == ["--startup-server"]
    then hSetBuffering stdout LineBuffering >> Lib.someFunc
    else hspec tests

tests :: Spec
tests = do
  WireSpec.spec
  QuerySpec.spec
  PlanSpec.spec
  ContractSpec.spec
  ApiSmokeSpec.spec
  EmployeeSmokeSpec.spec
  EmployeeSpec.spec
  DemoSmokeSpec.spec
  DeleteSmokeSpec.spec
  OrganizationsSmokeSpec.spec
  DiscoverySmokeSpec.spec
  StartupSmokeSpec.spec
  SQLiteStoreSpec.spec
  registrySpec
  describe "목표 활성화 불변식" $ do
    it "최종 책임자가 없으면 거부한다" $
      validateGoal goal Nothing (Just authority) `shouldBe` Left (NoOwner gid)
    it "권한 기록이 없으면 거부한다" $
      validateGoal goal (Just ownership) Nothing `shouldBe` Left (NoAuthority uid)
    it "필수 권한이 빠지면 거부한다" $
      activate (DraftGoal goal) ownership (revokePermission Pricing authority)
        `shouldBe` Left (MissingPermissions gid uid (Set.singleton Pricing))
    it "필요 예산 미만이면 거부한다" $
      activate (DraftGoal goal) ownership authority {authorityBudgetLimit = 99}
        `shouldBe` Left (InsufficientBudget gid 100 99)
    it "다른 사람의 권한으로 활성화할 수 없다" $
      activate (DraftGoal goal) ownership authority {authorityOwner = UserId "other"}
        `shouldBe` Left (OwnerMismatch gid uid (UserId "other"))
    it "책임자와 충분한 권한 및 KPI가 있으면 활성화한다" $
      fmap activeGoal (activate (DraftGoal goal) ownership authority) `shouldBe` Right goal
    it "빈 KPI와 설명을 거부한다" $ do
      validateDraft goal {goalDescription = " "} `shouldSatisfy` isLeft
      validateDraft goal {goalMetric = metric {metricName = " "}} `shouldSatisfy` isLeft
    it "KPI 방향과 목표 수치의 모순을 거부한다" $
      validateDraft goal {goalTarget = 50} `shouldSatisfy` isLeft
    it "음수 예산과 비유한 수치를 거부한다" $ do
      validateDraft goal {goalRequiredBudget = -1} `shouldSatisfy` isLeft
      validateDraft goal {goalTarget = 1 / 0} `shouldSatisfy` isLeft
      validateDraft goal {goalBaseline = 0 / 0} `shouldSatisfy` isLeft
    it "동일한 기준/목표와 역전된 날짜를 거부한다" $ do
      validateDraft goal {goalTarget = 100} `shouldBe` Left (InvalidTarget gid)
      validateDraft goal {goalDeadline = start} `shouldBe` Left (DeadlineBeforeStart gid)
    it "권한 플래그와 집합 모두에서 회수한다" $ do
      let a = authority {authorityCanApprove = Set.singleton Pricing}
      hasPermission (revokePermission Pricing a) Pricing `shouldBe` False
  describe "지표 평가" $ do
    it "증가 KPI의 중간값과 초과 달성을 계산한다" $ do
      progressOf goal 150 `shouldBe` 0.5
      evaluationStatus (evaluateGoal end goal [result 210 start]) `shouldBe` Achieved
    it "감소 KPI도 동일하게 계산한다" $ do
      let decreasing = goal {goalMetric = metric {metricDirection = LowerIsBetter}, goalTarget = 0}
      progressOf decreasing 50 `shouldBe` 0.5
      evaluationStatus (evaluateGoal end decreasing [result (-1) start]) `shouldBe` Achieved
    it "입력 순서가 아닌 보고 시각으로 최신 실측을 고른다" $
      evaluationLatestValue (evaluateGoal end goal [result 110 end, result 200 start])
        `shouldBe` Just 110
    it "결과가 없으면 NoData이고 지연 정도를 구분한다" $ do
      evaluationStatus (evaluateGoal end goal []) `shouldBe` NoData
      map (`classify` 1) [1, 0.95, 0.8, 0.1] `shouldBe` [Achieved, OnTrack, AtRisk, OffTrack]
    it "유한 실측값의 진척도는 0과 1 사이이다" $ property $ \(n :: Int) ->
      let p = progressOf goal (fromIntegral n) in p >= 0 && p <= 1
  describe "이벤트 재생" $ do
    it "책임자 없는 GoalActivated 이벤트로 활성화를 우회할 수 없다" $ do
      let st = applyEvent (replay (take 3 events)) (StoredEvent 4 start Nothing (GoalActivated gid))
      Set.member gid (stateActive st) `shouldBe` False
    it "예산 축소 후에는 활성 목표를 다시 검증한다" $ do
      let st =
            applyEvent
              ready
              (StoredEvent 7 end Nothing (AuthorityGranted uid authority {authorityBudgetLimit = 0}))
      Set.member gid (stateActive st) `shouldBe` False
    it "전체 재생과 부분 재생 후 이어 붙인 상태가 같다" $ do
      let (prefix, suffix) = splitAt 3 events
      applyEvents (replay prefix) suffix `shouldBe` replay events
    it "책임자를 변경하면 재검증 전 활성 상태를 해제한다" $ do
      let changed = applyEvent ready (StoredEvent 8 end Nothing (OwnerAssigned gid (UserId "other")))
      Set.member gid (stateActive changed) `shouldBe` False
    it "결과와 감사 순번을 보존한다" $ do
      let st = applyEvent ready (StoredEvent 8 end (Just uid) (ResultReported gid (result 150 end)))
      resultsOf st gid `shouldBe` [result 150 end]
      stateLastSeq st `shouldBe` 8
  describe "조직 컴파일러와 리뷰" $ do
    it "조직과 최종 책임자의 부재를 코드로 보고한다" $ do
      codes emptyState `shouldContain` ["O000"]
      codes (replay (take 3 events)) `shouldContain` ["O001"]
    it "권한 부족과 결과 없는 활성 목표를 진단한다" $ do
      codes ready `shouldContain` ["O050"]
      codes ready {stateAuthorities = Map.singleton uid (emptyAuthority uid)}
        `shouldContain` ["O017"]
    it "같은 KPI를 서로 다른 최종 책임자가 맡으면 경고한다" $ do
      let gid2 = GoalId "g2"
          st =
            ready
              { stateGoals = Map.insert gid2 goal {goalId = gid2} (stateGoals ready)
              , stateOwnership =
                  Map.insert gid2 (Ownership gid2 (UserId "other") start) (stateOwnership ready)
              }
      codes st `shouldContain` ["O020"]
    it "결정도 학습도 없는 리뷰와 기한 없는 결정을 경고한다" $ do
      checkReview review `shouldBe` [NoDecisionProduced]
      codes ready {stateReviews = [review]} `shouldContain` ["O040"]
      checkReview review {reviewLearnings = [Learning "학습"]} `shouldBe` []
      checkReview review {reviewDecisions = [Decision "가격 실험" uid Nothing]}
        `shouldBe` [DecisionWithoutDeadline "가격 실험"]
  describe "구조화된 분석과 표시 계약" $ do
    it "책임자 없음과 충분한 자원은 서로 다른 판단과 권고이다" $ do
      unowned <-
        either (fail . show) pure (Analysis.analyzeGoal ready {stateOwnership = Map.empty} gid)
      Analysis.analysisCause unowned `shouldBe` Analysis.OwnerMissing
      Analysis.analysisRecommendations unowned `shouldBe` [Analysis.AssignOwner]
      AnalysisView.possibleCause (AnalysisView.presentAnalysis unowned)
        `shouldBe` "이 목표에는 최종 책임자가 없습니다. 누구도 지연에 대해 답할 위치에 있지 않습니다."
      complete <- either (fail . show) pure (Analysis.analyzeGoal ready gid)
      Analysis.analysisCause complete `shouldBe` Analysis.ResourcesControlled uid
      Analysis.analysisCoverage complete `shouldBe` 1
      Analysis.analysisRecommendations complete `shouldBe` [Analysis.NoStructuralIssue]
      AnalysisView.possibleCause (AnalysisView.presentAnalysis complete)
        `shouldBe` "owner이(가) 목표 달성에 필요한 자원을 모두 통제하고 있습니다. 구조적 병목은 발견되지 않았으며, 실행 자체를 점검해야 합니다."
    it "부족 권한과 예산의 원자료를 보존하고 기존 분석 문구를 출력한다" $ do
      let lacking = ready {stateAuthorities = Map.singleton uid (emptyAuthority uid)}
      analysis <- either (fail . show) pure (Analysis.analyzeGoal lacking gid)
      Analysis.analysisCause analysis `shouldBe` Analysis.InsufficientAuthority uid "매출 성장" 0
      Analysis.analysisResources analysis
        `shouldBe` [ Analysis.ResourceHolder (Analysis.PermissionRequired Pricing) True False []
                   , Analysis.ResourceHolder (Analysis.BudgetRequired 100) True False []
                   ]
      Analysis.analysisRecommendations analysis
        `shouldBe` [ Analysis.IncreaseOwnerAuthority
                       uid
                       [Analysis.PermissionRequired Pricing, Analysis.BudgetRequired 100]
                   , Analysis.MoveAccountabilityUpward uid
                   ]
      AnalysisView.renderAnalysis analysis
        `shouldBe` "Possible cause\nowner owns 매출 성장. However, owner controls only 0% of the resources required to achieve the assigned goal.\n\nPricing authority -> (nobody)\nBudget 100 authority -> (nobody)\n\nRecommendation:\n1. increase owner authority (Pricing, Budget)\n2. move accountability for this goal upward from owner\n"
    it "모든 컴파일러 진단의 원인과 표시 계약을 보존한다" $
      mapM_
        ( \(st, expected, view) -> do
            reportDiagnostics (compileOrganization (addUTCTime 1 end) st) `shouldContain` [expected]
            DiagnosticView.presentDiagnostic expected `shouldBe` view
        )
        diagnosticCases
    it "회고 판단과 두 경고 문구를 각각 보존한다" $ do
      checkReview review `shouldBe` [NoDecisionProduced]
      describeReviewWarning NoDecisionProduced `shouldBe` "This review produced no decision."
      checkReview review {reviewDecisions = [Decision "다음 실험" uid Nothing]}
        `shouldBe` [DecisionWithoutDeadline "다음 실험"]
      describeReviewWarning (DecisionWithoutDeadline "다음 실험")
        `shouldBe` "결정에 기한이 없습니다: 다음 실험"
  describe "명령 처리 경계" $ do
    it "목표 권한은 일반 권한과 같은 단일 이벤트를 반환한다" $ do
      executeCommand start ready (GrantGoalAuthority gid authority)
        `shouldBe` Right [AuthorityGranted uid authority]
      executeCommand start ready (GrantGoalAuthority gid authority)
        `shouldBe` executeCommand start ready (GrantAuthority authority)
    it "권한 예산 검증보다 목표와 책임자 일치 및 사람 존재를 우선한다" $ do
      let missing = UserId "missing"
          invalidAuthority = authority {authorityOwner = missing, authorityBudgetLimit = -1}
      executeCommand start ready (GrantGoalAuthority (GoalId "missing") invalidAuthority)
        `shouldBe` Left (GoalNotFound (GoalId "missing"))
      executeCommand
        start
        ready {stateOwnership = Map.empty}
        (GrantGoalAuthority gid invalidAuthority)
        `shouldBe` Left (NoOwner gid)
      executeCommand start ready (GrantGoalAuthority gid invalidAuthority)
        `shouldBe` Left (OwnerMismatch gid uid missing)
      executeCommand
        start
        ready {statePeople = Map.empty}
        (GrantGoalAuthority gid authority {authorityBudgetLimit = -1})
        `shouldBe` Left (PersonNotFound uid)
      executeCommand start ready (GrantGoalAuthority gid authority {authorityBudgetLimit = -1})
        `shouldBe` Left (InvalidInput "예산은 음수일 수 없습니다.")
    it "없는 조직과 중복 ID를 거부한다" $ do
      executeCommand start emptyState (AddPerson person) `shouldSatisfy` isLeft
      executeCommand start ready (CreateGoal goal) `shouldSatisfy` isLeft
      executeCommand start ready (AddPerson person) `shouldSatisfy` isLeft
    it "없는 사람을 책임자로 지정하지 못한다" $
      executeCommand start ready (AssignOwner gid (UserId "missing")) `shouldSatisfy` isLeft
    it "없는 부모와 다른 조직의 목표를 거부한다" $ do
      executeCommand
        start
        ready
        (CreateGoal goal {goalId = GoalId "new", goalParent = Just (GoalId "missing")})
        `shouldSatisfy` isLeft
      executeCommand
        start
        ready
        (CreateGoal goal {goalId = GoalId "new", goalOrganization = OrgId "other"})
        `shouldSatisfy` isLeft
    it "없는 보고자와 비유한 결과를 거부한다" $ do
      executeCommand end ready (ReportResult gid 150 (UserId "missing") "보고")
        `shouldSatisfy` isLeft
      executeCommand end ready (ReportResult gid (1 / 0) uid "보고") `shouldSatisfy` isLeft
    it "정상 결과를 받아 평가 및 회고로 이어진다" $ do
      executeCommand end ready (ReportResult gid 200 uid "달성") `shouldSatisfy` isRight
      executeCommand end ready (EvaluateGoal gid) `shouldSatisfy` isRight
      executeCommand end ready (HoldReview (ReviewId "r2") gid [Learning "배움"] [] "회고")
        `shouldSatisfy` isRight
    it "없는 결정 담당자를 가진 회고를 거부한다" $
      executeCommand
        end
        ready
        (HoldReview (ReviewId "r2") gid [] [Decision "실험" (UserId "missing") Nothing] "회고")
        `shouldSatisfy` isLeft
    it "필수 권한을 회수한 뒤 무효한 활성 목표가 남지 않는다" $
      case executeCommand end ready (RevokeAuthority uid Pricing) of
        Left _ -> pure ()
        Right es -> do
          let st = applyEvents ready (zipWith (\n e -> StoredEvent n end (Just uid) e) [8 ..] es)
          Set.member gid (stateActive st) `shouldBe` False

  describe "이벤트 파일 저장소" $ do
    it "재개방해도 이벤트와 재생 상태가 동일하다" $ withStorePath $ \path -> do
      savedBefore <- bracket (openFileStore path) closeStore $ \store -> do
        runCommand store Nothing (CreateOrganization (OrgId "org") "조직")
          >>= (`shouldSatisfy` isRight)
        runCommand store Nothing (AddPerson person) >>= (`shouldSatisfy` isRight)
        readStore store
      bracket (openFileStore path) closeStore $ \store -> readStore store `shouldReturn` savedBefore
    it "손상 파일을 초기화하거나 덮어쓰지 않는다" $ withStorePath $ \path -> do
      BL.writeFile path "{corrupt"
      opened <- try (openFileStore path) :: IO (Either IOException Store)
      fmap (const ()) opened `shouldSatisfy` isLeft
      BL.readFile path `shouldReturn` "{corrupt"
    it "같은 파일의 두 번째 쓰기 프로세스를 막는다" $ withStorePath $ \path ->
      bracket (openFileStore path) closeStore $ \_ -> do
        opened <- try (openFileStore path) :: IO (Either IOException Store)
        fmap (const ()) opened `shouldSatisfy` isLeft
    it "디스크 저장 실패 시 메모리 상태가 바뀌지 않는다" $ withStorePath $ \path ->
      bracket (openFileStore path) closeStore $ \store -> do
        createDirectory path
        savedBefore <- readStore store
        runCommand store Nothing (CreateOrganization (OrgId "org") "조직")
          >>= (`shouldSatisfy` isLeft)
        readStore store `shouldReturn` savedBefore
        removeDirectory path
    it "동시 쓰기를 직렬화하고 감사 순번을 중복시키지 않는다" $ withStorePath $ \path ->
      bracket (openFileStore path) closeStore $ \store -> do
        runCommand store Nothing (CreateOrganization (OrgId "org") "조직")
          >>= (`shouldSatisfy` isRight)
        boxes <- forM ["alice", "bob", "charlie"] $ \name -> do
          box <- newEmptyMVar
          _ <-
            forkIO $
              runCommand store Nothing (AddPerson (Person (UserId name) name "역할" Nothing))
                >>= putMVar box
          pure box
        outcomes <- mapM takeMVar boxes
        outcomes `shouldSatisfy` all isRight
        (st, saved) <- readStore store
        Map.size (statePeople st) `shouldBe` 3
        map storedSeq saved `shouldBe` [1, 2, 3, 4]

  describe "데모 시나리오" $ do
    it "전체 시드는 유효한 활성 목표와 다섯 성과 상태를 만든다" $ withDemo start $ \st saved -> do
      Map.size (statePeople st) `shouldBe` 6
      Map.size (stateGoals st) `shouldBe` 7
      length (activeGoals st) `shouldBe` 5
      length (draftGoals st) `shouldBe` 2
      mapM_ (\g -> validateActive st (goalId g) `shouldSatisfy` isRight) (activeGoals st)
      demoStatuses start st `shouldBe` sort [NoData, OnTrack, AtRisk, OffTrack, Achieved]
      map storedSeq saved `shouldBe` [1 .. length saved]
    it "미지정·권한 부족·집중도·빈 회고 진단과 그래프를 담는다" $ withDemo start $ \st _ -> do
      codes st `shouldContain` ["O001"]
      codes st `shouldContain` ["O017"]
      codes st `shouldContain` ["O031"]
      codes st `shouldContain` ["O040"]
      Set.fromList (map edgeKind (graphEdges (buildGraph st)))
        `shouldBe` Set.fromList [Owns, DependsOn, Measures, Controls]
      length (goalsWithoutOwner st) `shouldBe` 1
      ownersLackingAuthority st `shouldSatisfy` (not . null)
      stateStrategies st `shouldSatisfy` (not . Map.null)
    it "수년 후 생성해도 같은 상태와 미래 기한을 유지한다" $
      mapM_
        ( \now -> withDemo now $ \st _ -> do
            demoStatuses now st `shouldBe` sort [NoData, OnTrack, AtRisk, OffTrack, Achieved]
            mapM_ (\g -> goalDeadline g `shouldSatisfy` (> now)) (Map.elems (stateGoals st))
            mapM_
              (\d -> decisionDeadline d `shouldSatisfy` maybe True (> now))
              (concatMap reviewDecisions (stateReviews st))
        )
        [start, addUTCTime (3650 * 86400) start]
    it "감사 시각을 꾸미지 않으며 actor는 이미 존재하는 사람이다" $ withDemo start $ \_ saved -> do
      map storedAt saved `shouldBe` replicate (length saved) start
      let checkActor st event = do
            mapM_ (\actor -> Map.member actor (statePeople st) `shouldBe` True) (storedActor event)
            pure (applyEvent st event)
      _ <- foldM checkActor emptyState saved
      pure ()
    it "한글과 전체 이벤트가 JSON 왕복 및 동일 기준 시각에서 보존된다" $ withDemo start $ \st saved -> do
      fmap organizationName (stateOrganization st) `shouldBe` Just "북극성 스튜디오 · 체험 조직"
      (eitherDecodeWire (encodeWire saved) :: Either String [StoredEvent])
        `shouldBe` Right saved
      demoEvents start `shouldBe` Right saved
  describe "원자적 데모 초기화" $ do
    it "전체 초기화와 재개방이 동일하며 재시드는 파일을 변경하지 않는다" $ withStorePath $ \path -> do
      original <- bracket (openFileStore path) closeStore $ \store -> do
        seedDemo store >>= (`shouldSatisfy` isRight)
        saved <- readStore store
        bytes <- BL.readFile path
        seedDemo store `shouldReturn` Left OrganizationAlreadyExists
        readStore store `shouldReturn` saved
        BL.readFile path `shouldReturn` bytes
        pure saved
      bracket (openFileStore path) closeStore $ \store -> do
        readStore store `shouldReturn` original
        seedDemo store `shouldReturn` Left OrganizationAlreadyExists
        readStore store `shouldReturn` original
    it "일반 기존 데이터를 보존하며 별도 데모를 추가한다" $ withStorePath $ \path ->
      bracket (openFileStore path) closeStore $ \store -> do
        runCommand store Nothing (CreateOrganization (OrgId "real-org") "기존 조직")
          >>= (`shouldSatisfy` isRight)
        original <- readRegistry store
        audit <- readAudit store
        seedDemo store >>= (`shouldSatisfy` isRight)
        registry <- readRegistry store
        organizationState registry (OrgId "real-org")
          `shouldBe` organizationState original (OrgId "real-org")
        length (activeOrganizations registry) `shouldBe` 2
        saved <- readAudit store
        take (length audit) saved `shouldBe` audit
    it "저장 실패 시 부분 시드가 없고 장애 해소 뒤 재시도할 수 있다" $ withStorePath $ \path ->
      bracket (openFileStore path) closeStore $ \store -> do
        createDirectory path
        seedDemo store `shouldReturn` Left StorageFailure
        readStore store `shouldReturn` (emptyState, [])
        listDirectory path `shouldReturn` []
        removeDirectory path
        seedDemo store >>= (`shouldSatisfy` isRight)
        (st, _) <- readStore store
        Map.size (stateGoals st) `shouldBe` 7
    it "동시 초기화 중 한 요청만 전체 시드를 저장한다" $ withStorePath $ \path ->
      bracket (openFileStore path) closeStore $ \store -> do
        boxes <- forM [1 .. 4 :: Int] $ \_ -> do
          box <- newEmptyMVar
          _ <- forkIO $ seedDemo store >>= putMVar box
          pure box
        outcomes <- mapM takeMVar boxes
        length (filter isRight outcomes) `shouldBe` 1
        length (filter (== Left OrganizationAlreadyExists) outcomes) `shouldBe` 3
        (st, saved) <- readStore store
        Map.size (stateGoals st) `shouldBe` 7
        map storedSeq saved `shouldBe` [1 .. length saved]

  describe "조직 논리 삭제" $ do
    it "삭제 시 현재 데이터 전체를 초기화하고 감사 순번을 유지한다" $ withDemo start $ \st saved -> do
      let org = maybe (error "missing demo") id (stateOrganization st)
      case executeCommand
        end
        st
        (DeleteOrganization (organizationId org) (organizationName org) (stateLastSeq st)) of
        Left err -> expectationFailure (show err)
        Right changes -> do
          let additions = zipWith (\n e -> StoredEvent n end Nothing e) [length saved + 1 ..] changes
              allEvents = saved <> additions
          replay allEvents `shouldBe` emptyState {stateLastSeq = length allEvents}
          currentEpoch allEvents `shouldBe` []
          take (length saved) allEvents `shouldBe` saved
    it "잘못된 대상·이름·버전 및 없는 조직의 삭제를 거부한다" $ do
      executeCommand end emptyState (DeleteOrganization (OrgId "org") "테스트 조직" 0)
        `shouldSatisfy` isLeft
      executeCommand end ready (DeleteOrganization (OrgId "wrong") "테스트 조직" 6)
        `shouldSatisfy` isLeft
      executeCommand end ready (DeleteOrganization (OrgId "org") "wrong" 6)
        `shouldSatisfy` isLeft
      executeCommand end ready (DeleteOrganization (OrgId "org") "테스트 조직" 5)
        `shouldSatisfy` isLeft
    it "동일 ID의 새 조직에는 과거 기록과 참조가 섞이지 않는다" $ do
      let deleted = events <> [StoredEvent 7 end Nothing (OrganizationDeleted (OrgId "org"))]
          created =
            StoredEvent 8 end Nothing (OrganizationCreated (Organization (OrgId "org") "테스트 조직" end))
          st = replay (deleted <> [created])
      currentEpoch (deleted <> [created]) `shouldBe` [created]
      Map.null (statePeople st) `shouldBe` True
      Map.null (stateGoals st) `shouldBe` True
      executeCommand end st (DeleteOrganization (OrgId "org") "테스트 조직" 6) `shouldSatisfy` isLeft
    it "데모 ID만 흉내 낸 일반 조직은 데모가 아니다" $ do
      let fake =
            [ StoredEvent
                1
                start
                Nothing
                (OrganizationCreated (Organization demoOrganizationId "직접 만든 조직" start))
            ]
      isDemoEpoch fake `shouldBe` False
      isDemoStore fake `shouldBe` False
    it "삭제 저장 실패 시 현재 상태와 감사 파일을 보존한다" $ withStorePath $ \path ->
      bracket (openFileStore path) closeStore $ \store -> do
        seedDemo store >>= (`shouldSatisfy` isRight)
        saved@(st, _) <- readStore store
        let org = maybe (error "missing demo") id (stateOrganization st)
        bytes <- BL.readFile path
        renameFile path (path <> ".saved")
        createDirectory path
        runCommand
          store
          Nothing
          (DeleteOrganization (organizationId org) (organizationName org) (stateLastSeq st))
          `shouldReturn` Left StorageFailure
        readStore store `shouldReturn` saved
        BL.readFile (path <> ".saved") `shouldReturn` bytes
        removeDirectory path
        renameFile (path <> ".saved") path
    it "삭제·재개방·재시드 후 전역 순번과 과거 감사가 보존된다" $ withStorePath $ \path -> do
      deleted <- bracket (openFileStore path) closeStore $ \store -> do
        seedDemo store >>= (`shouldSatisfy` isRight)
        (st, _) <- readStore store
        saved <- readAudit store
        let org = maybe (error "missing demo") id (stateOrganization st)
        runCommand
          store
          Nothing
          (DeleteOrganization (organizationId org) (organizationName org) (stateLastSeq st))
          >>= (`shouldSatisfy` isRight)
        audit <- readAudit store
        take (length saved) audit `shouldBe` saved
        pure audit
      bracket (openFileStore path) closeStore $ \store -> do
        readStore store `shouldReturn` (emptyState {stateLastSeq = length deleted}, deleted)
        seedDemo store >>= (`shouldSatisfy` isRight)
        audit <- readAudit store
        take (length deleted) audit `shouldBe` deleted
        map storedSeq audit `shouldBe` [1 .. length audit]
        registry <- readRegistry store
        map
          storedSeq
          (currentEpoch (Map.findWithDefault [] demoOrganizationId (registryEvents registry)))
          `shouldBe` [length deleted + 1 .. length audit]
    it "동일 확인 버전의 동시 삭제는 단 한 번만 성공한다" $ withStorePath $ \path ->
      bracket (openFileStore path) closeStore $ \store -> do
        runCommand store Nothing (CreateOrganization (OrgId "org") "가상 조직")
          >>= (`shouldSatisfy` isRight)
        boxes <- forM [1 .. 2 :: Int] $ \_ -> do
          box <- newEmptyMVar
          _ <-
            forkIO $
              runCommand store Nothing (DeleteOrganization (OrgId "org") "가상 조직" 1) >>= putMVar box
          pure box
        outcomes <- mapM takeMVar boxes
        length (filter isRight outcomes) `shouldBe` 1
        length (filter isLeft outcomes) `shouldBe` 1
        (st, audit) <- readStore store
        st `shouldBe` emptyState {stateLastSeq = 2}
        length audit `shouldBe` 2
    it "확인창 이후 변경된 조직은 오래된 확인으로 삭제되지 않는다" $ withStorePath $ \path ->
      bracket (openFileStore path) closeStore $ \store -> do
        runCommand store Nothing (CreateOrganization (OrgId "org") "가상 조직")
          >>= (`shouldSatisfy` isRight)
        runCommand store Nothing (AddPerson person) >>= (`shouldSatisfy` isRight)
        saved <- readStore store
        runCommand store Nothing (DeleteOrganization (OrgId "org") "가상 조직" 1)
          >>= (`shouldSatisfy` isLeft)
        readStore store `shouldReturn` saved

withDemo :: UTCTime -> (OrgState -> [StoredEvent] -> IO ()) -> IO ()
withDemo now action = case demoEvents now of
  Left err    -> expectationFailure (show err)
  Right saved -> action (replay saved) saved

demoStatuses :: UTCTime -> OrgState -> [GoalStatus]
demoStatuses now st =
  sort
    [evaluationStatus (evaluateGoal now g (resultsOf st (goalId g))) | g <- activeGoals st]

withStorePath :: (FilePath -> IO a) -> IO a
withStorePath = bracket acquire cleanup
  where
    acquire = do
      dir <- getTemporaryDirectory
      (path, handle) <- openTempFile dir "my-org-store-test.json"
      hClose handle
      removeFile path
      pure path
    cleanup path = do
      isFile <- doesFileExist path
      when isFile (removeFile path)
      isDir <- doesDirectoryExist path
      when isDir (removeDirectory path)
      locked <- doesDirectoryExist (path <> ".lock")
      when locked (removeDirectory (path <> ".lock"))

start, end :: UTCTime
start = read "2026-01-01 00:00:00 UTC"
end = addUTCTime (100 * 86400) start

gid :: GoalId
gid = GoalId "g1"
uid :: UserId
uid = UserId "owner"
metric :: Metric
metric = Metric (MetricId "revenue") "매출" "KRW" HigherIsBetter
goal :: Goal
goal =
  Goal
    gid
    (OrgId "org")
    "매출 성장"
    metric
    100
    200
    start
    end
    Nothing
    (Set.singleton Pricing)
    100
person :: Person
person = Person uid "책임자" "영업" Nothing
ownership :: Ownership
ownership = Ownership gid uid start
authority :: Authority
authority = (emptyAuthority uid) {authorityBudgetLimit = 100, authorityCanChangePrice = True}
result :: Double -> UTCTime -> Result
result value at = Result gid value at uid "실측"
review :: Review
review = Review (ReviewId "r1") gid Nothing (evaluateGoal end goal []) [] [] end "회고"
events :: [StoredEvent]
events =
  zipWith
    (\n e -> StoredEvent n start (Just uid) e)
    [1 ..]
    [ OrganizationCreated (Organization (OrgId "org") "테스트 조직" start)
    , PersonAdded person
    , GoalCreated goal
    , OwnerAssigned gid uid
    , AuthorityGranted uid authority
    , GoalActivated gid
    ]
ready :: OrgState
ready = replay events
codes :: OrgState -> [Text]
codes = map diagnosticCode . reportDiagnostics . compileOrganization start

diagnosticCases :: [(OrgState, Diagnostic, DiagnosticView.DiagnosticView)]
diagnosticCases =
  [ entry
      emptyState
      "O000"
      Error
      OrganizationSubject
      OrganizationMissing
      "organization"
      "조직이 정의되지 않았습니다."
      []
  , entry
      (changed goal {goalTarget = 100})
      "O002"
      Error
      goalSubject
      TargetEqualsBaseline
      goalText
      "목표값이 기준값과 같아 성공과 실패를 판단할 수 없습니다."
      []
  , entry
      (changed goal {goalDeadline = start})
      "O003"
      Error
      goalSubject
      DeadlinePrecedesStart
      goalText
      "마감이 시작일보다 앞섭니다."
      []
  , entry
      (changed goal {goalRequiredBudget = -1})
      "O009"
      Error
      goalSubject
      (InvalidDraft (InvalidInput "예산은 음수일 수 없습니다."))
      goalText
      "예산은 음수일 수 없습니다."
      []
  , entry
      ready {stateOwnership = Map.empty}
      "O001"
      Error
      goalSubject
      FinalOwnerMissing
      goalText
      "Final Owner가 존재하지 않습니다."
      []
  , entry
      ready {statePeople = Map.empty}
      "O010"
      Error
      (GoalIdSubject gid)
      (UnknownOwner uid)
      "g1"
      "책임자 owner이(가) 구성원 명단에 없습니다."
      []
  , entry
      ready {stateAuthorities = Map.empty}
      "O018"
      Error
      (PersonSubject uid)
      (AuthorityMissing gid "매출 성장")
      "owner"
      "목표 g1 \"매출 성장\"의 책임자이지만 권한 기록이 전혀 없습니다."
      []
  , entry
      ready {stateAuthorities = Map.singleton uid (emptyAuthority uid)}
      "O017"
      Warning
      (PersonSubject uid)
      (AuthorityInsufficient "매출 성장" 200 "KRW" 0 [Pricing] 0 100)
      "owner"
      "책임에 비해 권한이 부족합니다."
      [ "Responsibility: 매출 성장 = 200.0 KRW"
      , "Controls 0% of required resources"
      , "Pricing = False"
      , "Budget = 0 (required 100)"
      ]
  , entry
      shared
      "O020"
      Warning
      (MetricSubject (MetricId "revenue"))
      (SharedMetricOwnership [(gid2, other), (gid, uid)])
      "revenue"
      "두 명 이상이 동일한 결과를 최종 책임지고 있습니다."
      ["g2 -> other", "g1 -> owner"]
  , entry
      ready
        { stateOwnership =
            Map.fromList
              [(GoalId name, Ownership (GoalId name) uid start) | name <- ["g1", "g2", "g3", "g4"]]
        }
      "O021"
      Warning
      (PersonSubject uid)
      (OwnerOverloaded 4)
      "owner"
      "한 사람이 4개의 목표를 최종 책임지고 있습니다."
      []
  , entry
      ready
        { stateAuthorities = Map.insert other (emptyAuthority other) (stateAuthorities ready)
        }
      "O031"
      Warning
      (PersonSubject uid)
      (DecisionConcentration uid 1)
      "owner"
      "owner이(가) 전체 조직 의사결정 권한의 100%를 가지고 있습니다."
      ["Possible bottleneck detected."]
  , entry
      ready {stateReviews = [review]}
      "O040"
      Warning
      (ReviewSubject (ReviewId "r1"))
      (ReviewWithoutOutcome gid)
      "r1"
      "This review produced no decision."
      ["Goal: g1"]
  , entry
      ready
      "O050"
      Info
      goalSubject
      ActiveGoalWithoutResult
      goalText
      "활성화된 뒤 보고된 결과가 없습니다."
      []
  , entry
      ready
      "O051"
      Warning
      goalSubject
      GoalPastDeadline
      goalText
      "마감이 지났지만 목표가 달성되지 않았습니다."
      []
  ]
  where
    entry st code severity subject cause text message details =
      ( st
      , Diagnostic code severity subject cause
      , DiagnosticView.DiagnosticView code severity text message details
      )
    goalSubject = GoalSubject gid "매출 성장"
    goalText = "g1 \"매출 성장\""
    changed g = ready {stateGoals = Map.singleton gid g}
    gid2 = GoalId "g2"
    other = UserId "other"
    shared =
      ready
        { stateGoals = Map.insert gid2 goal {goalId = gid2} (stateGoals ready)
        , stateOwnership = Map.insert gid2 (Ownership gid2 other start) (stateOwnership ready)
        }
