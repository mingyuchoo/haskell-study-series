module RegistrySpec
  ( registrySpec
  ) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import Control.Monad (forM, when)
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft, isRight)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime, addUTCTime)
import MyOrg.Application
import MyOrg.Demo
import MyOrg.Domain.Event
import MyOrg.Registry
import MyOrg.Serialization.JSON (encodeWire)
import MyOrg.Store
import MyOrg.Types
import System.Directory
import System.IO (hClose, openTempFile)
import Test.Hspec

registrySpec :: Spec
registrySpec = describe "여러 조직 레지스트리" $ do
  it "A와 B 생성·이름 변경·A 삭제는 B 상태와 버전에 영향이 없다" $ temporary $ \path ->
    bracket (openFileStore path) closeStore $ \store -> do
      createTwo store
      a <- stateOf store aid
      b <- stateOf store bid
      ok $
        runOrganizationCommand store aid Nothing (RenameOrganization aid "새 A" (stateLastSeq a))
      stateOf store bid `shouldReturn` b
      a2 <- stateOf store aid
      fmap organizationName (stateOrganization a2) `shouldBe` Just "새 A"
      fmap organizationCreatedAt (stateOrganization a2)
        `shouldBe` fmap organizationCreatedAt (stateOrganization a)
      ok $
        runOrganizationCommand store aid Nothing (DeleteOrganization aid "새 A" (stateLastSeq a2))
      stateOf store bid `shouldReturn` b
      registry <- readRegistry store
      map (fmap organizationId . stateOrganization) (activeOrganizations registry)
        `shouldBe` [Just bid]
  it "B 변경은 A 확인 버전을 만료시키지 않지만 같은 A 변경은 충돌한다" $ temporary $ \path ->
    bracket (openFileStore path) closeStore $ \store -> do
      createTwo store
      a <- stateOf store aid
      ok $ runOrganizationCommand store bid Nothing (AddPerson person)
      ok $
        runOrganizationCommand store aid Nothing (RenameOrganization aid "A 수정" (stateLastSeq a))
      runOrganizationCommand
        store
        aid
        Nothing
        (RenameOrganization aid "오래된 수정" (stateLastSeq a))
        >>= (`shouldSatisfy` isLeft)
  it "같은 사람·목표·회고 ID가 조직별로 독립적인 결과와 권한을 가진다" $ temporary $ \path ->
    bracket (openFileStore path) closeStore $ \store -> do
      createTwo store
      forM_ [aid, bid] $ \oid -> do
        ok $ runOrganizationCommand store oid Nothing (AddPerson person)
        ok $ runOrganizationCommand store oid Nothing (GrantAuthority (emptyAuthority uid))
        ok $ runOrganizationCommand store oid Nothing (CreateGoal (goal oid))
        ok $ runOrganizationCommand store oid Nothing (AssignOwner gid uid)
        ok $ runOrganizationCommand store oid Nothing (ActivateGoal gid)
        ok $
          runOrganizationCommand
            store
            oid
            Nothing
            (ReportResult gid (if oid == aid then 20 else 80) uid "실측")
        ok $
          runOrganizationCommand
            store
            oid
            Nothing
            (HoldReview (ReviewId "same-review") gid [Learning "학습"] [] "회고")
      a <- stateOf store aid
      b <- stateOf store bid
      map resultValue (resultsOf a gid) `shouldBe` [20]
      map resultValue (resultsOf b gid) `shouldBe` [80]
      length (stateReviews a) `shouldBe` 1
      length (stateReviews b) `shouldBe` 1
      ok $
        runOrganizationCommand
          store
          aid
          Nothing
          (GrantAuthority (Authority uid 500 True False Set.empty))
      stateOf store bid `shouldReturn` b
  it "다른 조직의 actor·owner·reporter·decisionOwner·reportsTo·parent를 거부한다" $ temporary $ \path ->
    bracket (openFileStore path) closeStore $ \store -> do
      createTwo store
      ok $
        runOrganizationCommand
          store
          aid
          Nothing
          (AddPerson (Person (UserId "a-only") "A 전용" "담당자" Nothing))
      ok $
        runOrganizationCommand
          store
          aid
          Nothing
          (CreateGoal (goal aid) {goalId = GoalId "a-only-goal"})
      ok $ runOrganizationCommand store bid Nothing (AddPerson person)
      ok $ runOrganizationCommand store bid Nothing (GrantAuthority (emptyAuthority uid))
      ok $ runOrganizationCommand store bid Nothing (CreateGoal (goal bid))
      ok $ runOrganizationCommand store bid Nothing (AssignOwner gid uid)
      ok $ runOrganizationCommand store bid Nothing (ActivateGoal gid)
      let other = UserId "a-only"
      runOrganizationCommand store bid (Just other) (EvaluateGoal gid)
        >>= (`shouldSatisfy` isLeft)
      forM_
        [ AssignOwner gid other
        , ReportResult gid 10 other "실측"
        , HoldReview (ReviewId "r") gid [] [Decision "결정" other Nothing] "회고"
        , AddPerson (Person (UserId "new") "새 사람" "역할" (Just other))
        , CreateGoal (goal bid) {goalId = GoalId "child", goalParent = Just (GoalId "a-only-goal")}
        , CreateGoal (goal aid) {goalId = GoalId "wrong-org"}
        ]
        $ \command -> runOrganizationCommand store bid Nothing command >>= (`shouldSatisfy` isLeft)
  it "동시 이름 변경과 삭제에서 같은 버전은 한 번만 성공한다" $ temporary $ \path ->
    bracket (openFileStore path) closeStore $ \store -> do
      createTwo store
      a <- stateOf store aid
      boxes <- forM
        [ RenameOrganization aid "변경" (stateLastSeq a)
        , DeleteOrganization aid "A 조직" (stateLastSeq a)
        ]
        $ \command -> do
          box <- newEmptyMVar
          _ <- forkIO $ runOrganizationCommand store aid Nothing command >>= putMVar box
          pure box
      outcomes <- mapM takeMVar boxes
      length (filter isRight outcomes) `shouldBe` 1
      length (filter isLeft outcomes) `shouldBe` 1
  it "저장 실패 시 모든 조직과 파일이 그대로 남는다" $ temporary $ \path ->
    bracket (openFileStore path) closeStore $ \store -> do
      createTwo store
      a <- stateOf store aid
      original <- readRegistry store
      bytes <- BL.readFile path
      renameFile path (path <> ".saved")
      createDirectory path
      runOrganizationCommand store aid Nothing (RenameOrganization aid "실패" (stateLastSeq a))
        `shouldReturn` Left StorageFailure
      readRegistry store `shouldReturn` original
      BL.readFile (path <> ".saved") `shouldReturn` bytes
      removeDirectory path
      renameFile (path <> ".saved") path
  it "다른 조직과 함께 데모 생성·중복거절·삭제후재시드가 가능하다" $ temporary $ \path ->
    bracket (openFileStore path) closeStore $ \store -> do
      ok $ runCommand store Nothing (CreateOrganization bid "B 조직")
      b <- stateOf store bid
      ok $ seedDemo store
      stateOf store bid `shouldReturn` b
      seedDemo store >>= (`shouldSatisfy` isLeft)
      demo <- stateOf store demoOrganizationId
      let org = maybe (error "missing demo") id (stateOrganization demo)
      ok $
        runOrganizationCommand
          store
          demoOrganizationId
          Nothing
          (DeleteOrganization demoOrganizationId (organizationName org) (stateLastSeq demo))
      ok $ seedDemo store
      stateOf store bid `shouldReturn` b
  it "구형51/52 이벤트는 읽기만으로 파일을 수정하지 않는다" $
    case demoEvents now of
      Left err -> expectationFailure (show err)
      Right marked -> forM_ [init marked, marked] $ \legacy -> temporary $ \path -> do
        BL.writeFile path (encodeWire legacy)
        bytes <- BL.readFile path
        bracket (openFileStore path) closeStore $ \store -> do
          st <- stateOf store demoOrganizationId
          Map.size (stateGoals st) `shouldBe` 7
          registry <- readRegistry store
          isDemoEpoch (Map.findWithDefault [] demoOrganizationId (registryEvents registry))
            `shouldBe` True
        BL.readFile path `shouldReturn` bytes
  it "scoped 이벤트는 구형 로그의 소속 커서를 바꾸지 않는다" $ do
    let stream =
          zipWith
            (\n e -> StoredEvent n now Nothing e)
            [1 ..]
            [ OrganizationCreated (Organization aid "A 조직" now)
            , OrganizationScoped bid (OrganizationCreated (Organization bid "B 조직" now))
            , PersonAdded person
            , OrganizationScoped bid (PersonAdded (Person (UserId "b-only") "B 전용" "역할" Nothing))
            ]
    case replayRegistry stream of
      Left err -> expectationFailure (show err)
      Right registry -> do
        fmap (Map.keys . statePeople) (organizationState registry aid) `shouldBe` Right [uid]
        fmap (Map.keys . statePeople) (organizationState registry bid)
          `shouldBe` Right [UserId "b-only"]
        registryLastSeq registry `shouldBe` 4
  it "구형 삭제/동일ID 재생성은 과거 데이터를 섞지 않는다" $ do
    let stream =
          zipWith
            (\n e -> StoredEvent n now Nothing e)
            [1 ..]
            [ OrganizationCreated (Organization aid "A 조직" now)
            , PersonAdded person
            , OrganizationDeleted aid
            , OrganizationCreated (Organization aid "새 A" now)
            ]
    case replayRegistry stream of
      Left err -> expectationFailure (show err)
      Right registry -> do
        fmap (Map.null . statePeople) (organizationState registry aid) `shouldBe` Right True
        map storedSeq (currentEpoch (Map.findWithDefault [] aid (registryEvents registry)))
          `shouldBe` [4]
  it "중첩 scope와 payload 조직 불일치 로그를 거부한다" $ do
    let bad e = replayRegistry [StoredEvent 1 now Nothing e]
    bad
      ( OrganizationScoped
          aid
          (OrganizationScoped aid (OrganizationCreated (Organization aid "A" now)))
      )
      `shouldSatisfy` isLeft
    bad (OrganizationScoped aid (OrganizationCreated (Organization bid "B" now)))
      `shouldSatisfy` isLeft

ok :: IO (Either OrganizationError a) -> IO ()
ok action = action >>= either (expectationFailure . show) (const (pure ()))
stateOf :: Store -> OrgId -> IO OrgState
stateOf store oid =
  readRegistry store
    >>= either (\err -> expectationFailure (show err) >> pure emptyState) pure
      . (`organizationState` oid)
createTwo :: Store -> IO ()
createTwo store = do
  ok $ runCommand store Nothing (CreateOrganization aid "A 조직")
  ok $ runCommand store Nothing (CreateOrganization bid "B 조직")
aid, bid :: OrgId
aid = OrgId "a"
bid = OrgId "b"
uid :: UserId
uid = UserId "same-person"
gid :: GoalId
gid = GoalId "same-goal"
person :: Person
person = Person uid "같은 식별자의 사람" "담당자" Nothing
now :: UTCTime
now = read "2026-09-01 00:00:00 UTC"
goal :: OrgId -> Goal
goal oid =
  Goal
    gid
    oid
    "테스트 목표"
    (Metric (MetricId "metric") "지표" "점" HigherIsBetter)
    0
    100
    now
    (addUTCTime (365 * 86400) now)
    Nothing
    Set.empty
    0
forM_ :: [a] -> (a -> IO b) -> IO ()
forM_ xs f = mapM_ (\x -> f x >> pure ()) xs
temporary :: (FilePath -> IO a) -> IO a
temporary = bracket acquire cleanup
  where
    acquire = do
      dir <- getTemporaryDirectory
      (path, h) <- openTempFile dir "my-org-registry-test.json"
      hClose h
      removeFile path
      pure path
    cleanup path = do
      exists <- doesFileExist path
      when exists (removeFile path)
      directory <- doesDirectoryExist path
      when directory (removeDirectory path)
      locked <- doesDirectoryExist (path <> ".lock")
      when locked (removeDirectory (path <> ".lock"))
