module OrganizationsSmokeSpec
  ( spec
  ) where

import Control.Monad (forM_, void)
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as T
import SmokeSupport
import Test.Hspec

spec :: Spec
spec = describe "HTTP multiple organizations" $ it "isolates scoped CRUD, actors, references, revisions and demo data" $ withFreshServer $ \client -> do
  let send path body status = void (post client path body status)
      org oid suffix = "organizations/" <> oid <> if null suffix then "" else "/" <> suffix
      person identifier = object ["id" .= String identifier, "name" .= String "가상 담당자", "role" .= String "역할"]
      routes = ["dashboard", "people", "goals", "events", "reviews", "compiler", "graph"]
  (items <$> get client "organizations") `shouldReturn` []
  send "organizations" (object ["id" .= String "a", "name" .= String "A 가상 조직"]) 201
  initial <- get client "dashboard"
  field (field initial "organization") "id" `shouldBe` String "a"
  send "organizations" (object ["id" .= String "b", "name" .= String "B 가상 조직"]) 201
  (length . items <$> get client "organizations") `shouldReturn` 2
  send "organizations" (object ["id" .= String "a", "name" .= String "중복"]) 409
  forM_ routes $ \path -> void (call client "GET" path Nothing 409)
  send "people" (person "ambiguous") 409
  a <- get client (org "a" "")
  _ <- get client (org "b" "dashboard")
  send (org "b" "people") (person "b-only") 201
  let rename name = object ["name" .= String name, "expectedVersion" .= field a "version"]
  _ <- call client "PATCH" (org "a" "") (Just (rename "A 새 이름")) 200
  _ <- call client "PATCH" (org "a" "") (Just (rename "오래된 이름")) 409
  renamed <- get client (org "a" "")
  field (field renamed "organization") "name" `shouldBe` String "A 새 이름"
  forM_ ["reportsTo", "actor"] $ \key -> send (org "a" "people") (merge (person "new") (object [key .= String "b-only"])) 404
  forM_ ["a", "b"] $ \oid -> do
    send (org oid "people") (person "same") 201
    send (org oid "goals") (goal "same-goal" (T.pack oid)) 201
    send (org oid "people/same/authority") (authority "same" 0 False) 201
    send (org oid "goals/same-goal/owner") (object ["owner" .= String "same"]) 201
    send (org oid "goals/same-goal/activate") empty 201
    send
      (org oid "goals/same-goal/results")
      ( object
          [ "value" .= (if oid == "a" then 20 else 80 :: Int)
          , "reportedBy" .= String "same"
          , "note" .= String "결과"
          ]
      )
      201
    send (org oid "evaluations") (object ["goal" .= String "same-goal"]) 201
    send
      (org oid "reviews")
      ( object
          [ "id" .= String "same-review"
          , "goal" .= String "same-goal"
          , "note" .= String "회고"
          , "learnings" .= [object ["text" .= (T.pack oid <> " 학습")]]
          ]
      )
      201
    mapM_ (get client . org oid) routes
  forM_ [("a", 20), ("b", 80)] $ \(oid, expected) -> do
    goals <- items <$> get client (org oid "goals")
    field (field (first goals) "evaluation") "latestValue" `shouldBe` Number expected
  send (org "a" "goals") (goal "wrong" "b") 400
  send (org "a" "goals/same-goal/owner") (object ["owner" .= String "b-only"]) 404
  send
    (org "a" "goals/same-goal/results")
    (object ["value" .= Number 2, "reportedBy" .= String "b-only", "note" .= String "결과"])
    404
  send
    (org "a" "reviews")
    ( object
        [ "id" .= String "invalid"
        , "goal" .= String "same-goal"
        , "note" .= String "회고"
        , "decisions" .= [object ["text" .= String "결정", "owner" .= String "b-only"]]
        ]
    )
    404
  b <- stable <$> get client (org "b" "dashboard")
  currentA <- get client (org "a" "")
  _ <-
    call
      client
      "DELETE"
      (org "a" "")
      ( Just
          (object ["confirmName" .= String "A 새 이름", "expectedVersion" .= field currentA "version"])
      )
      200
  (stable <$> get client (org "b" "dashboard")) `shouldReturn` b
  _ <- call client "GET" (org "a" "") Nothing 404
  (length . items <$> get client "organizations") `shouldReturn` 1
  send "organizations" (object ["id" .= String "a", "name" .= String "A 재생성"]) 201
  (items <$> get client (org "a" "people")) `shouldReturn` []
  (length . items <$> get client (org "a" "events")) `shouldReturn` 1
  _ <-
    call
      client
      "DELETE"
      (org "a" "")
      ( Just
          (object ["confirmName" .= String "A 재생성", "expectedVersion" .= field currentA "version"])
      )
      409
  send "demo" empty 201
  (stable <$> get client (org "b" "dashboard")) `shouldReturn` b
  send "demo" empty 409
  (length . items <$> get client "organizations") `shouldReturn` 3

merge :: Value -> Value -> Value
merge (Object original) (Object additions) = Object (KM.union additions original)
merge _ _                                  = error "Expected objects"

-- Evaluation uses the current clock; all persisted projection fields must remain identical.
stable :: Value -> Value
stable value@(Object fields) = Object (KM.insert "goals" stripped fields)
  where
    stripped = field (object ["values" .= map strip (items (field value "goals"))]) "values"
    strip (Object entry) = Object (KM.delete "evaluation" entry)
    strip entry          = entry
stable value = value
