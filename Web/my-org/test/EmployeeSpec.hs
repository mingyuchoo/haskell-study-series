module EmployeeSpec
  ( spec
  ) where

import Control.Exception (bracket)
import Data.Either (isLeft, isRight)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime)
import MyOrg.Application
import MyOrg.Domain.Event
import MyOrg.Domain.Organization
import MyOrg.Serialization.JSON (eitherDecodeWire, encodeWire)
import MyOrg.Store
import MyOrg.Types
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "직원 명령과 저장 재생" $ do
  it "빈 프로필은 기존 사람 이벤트를 유지하고 유효한 프로필만 정규화한다" $ do
    let employee = Person (UserId "new") "새 직원" "개발" Nothing
    executeCommand now baseState (AddEmployee employee emptyProfile)
      `shouldBe` Right [PersonAdded employee]
    executeCommand
      now
      baseState
      (AddEmployee employee (EmployeeProfile (Just "  ") (Just "\t")))
      `shouldBe` Right [PersonAdded employee]
    executeCommand
      now
      baseState
      (AddEmployee employee (EmployeeProfile (Just " 개발 ") (Just " staff@example.com ")))
      `shouldBe` Right [EmployeeAdded employee (EmployeeProfile (Just "개발") (Just "staff@example.com"))]
  it "직원 프로필 오류보다 조직과 기본정보 및 보고 대상 검증을 우선한다" $ do
    let employee = Person (UserId "new") "새 직원" "개발" Nothing
        invalidProfile = EmployeeProfile Nothing (Just "invalid-email")
        add st p = executeCommand now st (AddEmployee p invalidProfile)
        missing = UserId "missing"
    add emptyState employee `shouldBe` Left NoOrganization
    add baseState employee {personName = " "}
      `shouldBe` Left (InvalidInput "텍스트는 1~10000자여야 합니다.")
    add baseState employee {personId = boss} `shouldBe` Left (DuplicateId "boss")
    add baseState employee {personReportsTo = Just missing}
      `shouldBe` Left (PersonNotFound missing)
    add baseState employee `shouldBe` Left (InvalidInput "이메일 형식을 확인해주세요.")
  it "직속 후임을 승격해 자기 보고를 방지하고 모든 변경을 한 이벤트로 적용한다" $ do
    let st = baseState
        result = executeCommand now st (DeactivatePerson boss (Just junior) (stateLastSeq st))
    changes <- either (fail . show) pure result
    length changes `shouldBe` 1
    let next = applyEvents st (zipWith (\n e -> StoredEvent n now Nothing e) [5 ..] changes)
    Set.member boss (stateInactivePeople next) `shouldBe` True
    fmap personReportsTo (Map.lookup junior (statePeople next)) `shouldBe` Just Nothing
    fmap personReportsTo (Map.lookup peer (statePeople next)) `shouldBe` Just (Just junior)
    Map.lookup boss (statePeople next) `shouldSatisfy` maybe False ((== "부서장") . personName)
  it "일반 수정의 보고 순환과 없는 또는 비활성 후임을 거부한다" $ do
    executeCommand
      now
      baseState
      (UpdatePerson (Person boss "부서장" "리드" (Just junior)) emptyProfile 4)
      `shouldSatisfy` isLeft
    executeCommand now baseState (DeactivatePerson boss (Just (UserId "missing")) 4)
      `shouldSatisfy` isLeft
    let inactive = baseState {stateInactivePeople = Set.singleton junior}
    executeCommand now inactive (DeactivatePerson boss (Just junior) 4) `shouldSatisfy` isLeft
  it "프로필 수정과 비활성화를 저장 후 재개방해 동일하게 복원한다" $
    withSystemTempDirectory "my-org-employee-" $ \dir -> do
      let path = dir </> "events.json"
          profile = EmployeeProfile (Just "플랫폼") (Just "staff@example.com")
      saved <- bracket (openFileStore path) closeStore $ \store -> do
        runCommand store Nothing (CreateOrganization (OrgId "o") "조직")
          >>= (`shouldSatisfy` isRight)
        runCommand store Nothing (AddEmployee (Person boss "직원" "개발" Nothing) profile)
          >>= (`shouldSatisfy` isRight)
        (st, _) <- readStore store
        runCommand
          store
          Nothing
          (UpdatePerson (Person boss "새 이름" "리드" Nothing) profile (stateLastSeq st))
          >>= (`shouldSatisfy` isRight)
        (updated, _) <- readStore store
        runCommand store Nothing (DeactivatePerson boss Nothing (stateLastSeq updated))
          >>= (`shouldSatisfy` isRight)
        result@(finalState, events) <- readStore store
        Set.member boss (stateInactivePeople finalState) `shouldBe` True
        Map.lookup boss (stateProfiles finalState) `shouldBe` Just profile
        eitherDecodeWire (encodeWire events) `shouldBe` Right events
        pure result
      bracket (openFileStore path) closeStore $ \store -> readStore store `shouldReturn` saved

now :: UTCTime
now = read "2026-01-01 00:00:00 UTC"

boss, junior, peer :: UserId
boss = UserId "boss"
junior = UserId "junior"
peer = UserId "peer"

baseState :: OrgState
baseState =
  replay
    ( zipWith
        (\n e -> StoredEvent n now Nothing e)
        [1 ..]
        [ OrganizationCreated (Organization (OrgId "o") "조직" now)
        , PersonAdded (Person boss "부서장" "리드" Nothing)
        , PersonAdded (Person junior "후임" "개발" (Just boss))
        , PersonAdded (Person peer "동료" "개발" (Just boss))
        ]
    )
