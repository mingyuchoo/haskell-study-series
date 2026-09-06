module EmployeeSmokeSpec
  ( spec
  ) where

import Control.Monad (void)
import SmokeSupport
import Test.Hspec hiding (after, before)

spec :: Spec
spec = describe "직원 관리 HTTP 회귀" $ do
  it "기본정보 수정과 원자적 인계 후 과거 기록을 보존하고 신규 배정을 거부한다" $ withFreshServer $ \client -> do
    let send path body = void (post client path body 201)
        version = field <$> get client "dashboard" <*> pure "version"
        person identifier name = object ["id" .= String identifier, "name" .= String name, "role" .= String "개발"]
    send "organizations" (object ["id" .= String "org", "name" .= String "직원 테스트"])
    send "people" (person "old" "기존 담당자")
    send "people" (person "next" "후임")
    send
      "people"
      ( object
          [ "id" .= String "report"
          , "name" .= String "직속"
          , "role" .= String "개발"
          , "reportsTo" .= String "old"
          ]
      )
    v <- version
    void
      ( call
          client
          "PATCH"
          "people/old"
          ( Just
              ( object
                  [ "name" .= String "수정 이름"
                  , "role" .= String "리드"
                  , "department" .= String "플랫폼"
                  , "email" .= String "lead@example.com"
                  , "expectedVersion" .= v
                  ]
              )
          )
          200
      )
    detail <- get client "people/old"
    field (field detail "person") "department" `shouldBe` String "플랫폼"
    field (field detail "person") "name" `shouldBe` String "수정 이름"
    send "goals" (goal "g" "org")
    send "goals/g/owner" (object ["owner" .= String "old"])
    send "people/old/authority" (authority "old" 0 False)
    send "goals/g/activate" empty
    send
      "goals/g/results"
      (object ["value" .= Number 50, "reportedBy" .= String "old", "note" .= String "과거 실적"])
    send
      "reviews"
      ( object
          [ "id" .= String "r"
          , "goal" .= String "g"
          , "note" .= String "이전 회고"
          , "decisions" .= [object ["text" .= String "실험", "owner" .= String "old"]]
          ]
      )
    oldGoals <- get client "goals"
    oldReviews <- get client "reviews"
    before <- get client "events"
    v2 <- version
    (missingStatus, _) <-
      request client "POST" "people/old/deactivate" (Just (object ["expectedVersion" .= v2]))
    missingStatus `shouldBe` 400
    get client "events" `shouldReturn` before
    send
      "people/old/deactivate"
      (object ["successor" .= String "next", "expectedVersion" .= v2])
    old <- get client "people/old"
    field (field old "person") "status" `shouldBe` String "inactive"
    field old "ownedGoals" `shouldBe` field (object ["values" .= ([] :: [Value])]) "values"
    next <- get client "people/next"
    items (field next "ownedGoals") `shouldBe` [String "g"]
    report <- get client "people/report"
    field (field report "person") "reportsTo" `shouldBe` String "next"
    gs <- items <$> get client "goals"
    field (first gs) "active" `shouldBe` Bool False
    field (first gs) "results" `shouldBe` field (first (items oldGoals)) "results"
    get client "reviews" `shouldReturn` oldReviews
    dashboard <- get client "dashboard"
    map (`field` "owner") (items (field dashboard "authorities"))
      `shouldNotContain` [String "old"]
    audit <- items <$> get client "events"
    take (length (items before)) audit `shouldBe` items before
    (assignStatus, _) <-
      request client "POST" "goals/g/owner" (Just (object ["owner" .= String "old"]))
    assignStatus `shouldBe` 400
    (authorityStatus, _) <-
      request client "POST" "people/old/authority" (Just (authority "old" 0 False))
    authorityStatus `shouldBe` 400
  it "낡은 수정 버전과 자기 인계를 거부해 상태를 보존한다" $ withFreshServer $ \client -> do
    void
      (post client "organizations" (object ["id" .= String "org", "name" .= String "조직"]) 201)
    void
      ( post
          client
          "people"
          (object ["id" .= String "one", "name" .= String "원본", "role" .= String "역할"])
          201
      )
    before <- get client "dashboard"
    (stale, _) <-
      request
        client
        "PATCH"
        "people/one"
        ( Just
            (object ["name" .= String "변경", "role" .= String "역할", "expectedVersion" .= Number 0])
        )
    stale `shouldBe` 409
    (self, _) <-
      request
        client
        "POST"
        "people/one/deactivate"
        (Just (object ["successor" .= String "one", "expectedVersion" .= field before "version"]))
    self `shouldBe` 400
    after <- get client "dashboard"
    field after "people" `shouldBe` field before "people"
    field after "version" `shouldBe` field before "version"
  it "조직 범위 직원 조회와 수정은 다른 조직의 직원을 노출하지 않는다" $ withFreshServer $ \client -> do
    void (post client "organizations" (object ["id" .= String "a", "name" .= String "A"]) 201)
    void (post client "organizations" (object ["id" .= String "b", "name" .= String "B"]) 201)
    void
      ( post
          client
          "organizations/a/people"
          (object ["id" .= String "a-person", "name" .= String "A 직원", "role" .= String "개발"])
          201
      )
    detail <- get client "organizations/a/people/a-person"
    field (field detail "person") "name" `shouldBe` String "A 직원"
    void (call client "GET" "organizations/b/people/a-person" Nothing 404)
    b <- get client "organizations/b/dashboard"
    void
      ( call
          client
          "PATCH"
          "organizations/b/people/a-person"
          ( Just
              ( object
                  ["name" .= String "침범", "role" .= String "역할", "expectedVersion" .= field b "version"]
              )
          )
          404
      )
    get client "organizations/a/people/a-person" `shouldReturn` detail
