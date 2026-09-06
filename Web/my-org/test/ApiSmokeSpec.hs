module ApiSmokeSpec (spec) where

import Control.Monad (void)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import SmokeSupport
import Test.Hspec

spec :: Spec
spec = describe "HTTP API lifecycle" $ it "preserves validation, evaluation, reviews, events and authority revocation" $ withFreshServer $ \client -> do
  let send path body status = void (post client path body status)
      owner value = object ["owner" .= (value :: Text)]
      auth = authority "owner" 100 True
      revenue = case goal "revenue" "org" of
        Object fields -> Object (KM.union (KM.fromList [("baseline", Number 100), ("target", Number 200), ("requiredBudget", Number 100), ("requiredPermissions", toArray [String "Pricing"])]) fields)
        value -> value
  send "organizations" (object ["id" .= String "org", "name" .= String "HTTP 테스트 조직"]) 201
  send "organizations" (object ["id" .= String "org", "name" .= String "중복"]) 409
  send "people" (object ["id" .= String "owner", "name" .= String "<img src=x onerror=alert(1)>", "role" .= String "영업"]) 201
  send "goals" revenue 201
  send "goals" revenue 409
  send "goals/revenue/activate" empty 400
  send "goals/revenue/owner" (owner "missing") 404
  send "goals/revenue/owner" (owner "owner") 201
  send "goals/revenue/activate" empty 400
  send "people/owner/authority" auth 201
  send "goals/revenue/activate" (object ["actor" .= String "missing"]) 404
  send "goals/revenue/activate" (object ["actor" .= String "owner"]) 201
  send "goals/revenue/activate" empty 409
  send "goals/revenue/results" (object ["value" .= Number 200, "reportedBy" .= String "owner", "note" .= String "달성", "actor" .= String "owner"]) 201
  send "evaluations" (object ["goal" .= String "revenue"]) 201
  send "reviews" (object ["id" .= String "review1", "goal" .= String "revenue", "note" .= String "학습 없는 회고"]) 201
  send "reviews" (object ["id" .= String "badreview", "goal" .= String "revenue", "note" .= String "잘못된 결정", "decisions" .= [object ["text" .= String "실험", "owner" .= String "missing"]]]) 404
  send "reviews" (object ["id" .= String "review2", "goal" .= String "revenue", "note" .= String "다음 실험", "learnings" .= [object ["text" .= String "가격 권한 위임이 효과적"]], "decisions" .= [object ["text" .= String "가격 실험", "owner" .= String "owner", "deadline" .= String "2027-02-01T00:00:00Z"]]]) 201
  send "goals/revenue/strategy" (object ["note" .= String "기업 고객 확대"]) 201
  state <- get client "dashboard"
  field (field (first (items (field state "goals"))) "evaluation") "status" `shouldBe` String "Achieved"
  length (items (field state "reviews")) `shouldBe` 2
  map (`field` "code") (items (field (field state "compiler") "diagnostics")) `shouldContain` [String "O040"]
  field (first (items (field state "people"))) "name" `shouldBe` String "<img src=x onerror=alert(1)>"
  mapM_ (get client) ["organization", "people", "goals", "compiler", "graph", "events", "reviews"]
  events <- items <$> get client "events"
  map (number . (`field` "seq")) events `shouldBe` [1 .. length events]
  events `shouldSatisfy` any (\case Object fields -> KM.lookup "actor" fields == Just (String "owner"); _ -> False)
  send "people/owner/authority" (authority "owner" 100 False) 201
  goals <- items <$> get client "goals"
  field (first goals) "active" `shouldBe` Bool False
  send "goals/revenue/results" (object ["value" .= Number 200, "reportedBy" .= String "owner", "note" .= String "권한 회수 후"]) 409
  send "people/owner/authority" auth 201
  send "goals/revenue/activate" empty 201
  void (call client "GET" "missing" Nothing 404)
 where
  toArray values = field (object ["values" .= values]) "values"
