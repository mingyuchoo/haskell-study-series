module DeleteSmokeSpec (spec) where

import Control.Monad (void)
import qualified Data.Text as T
import SmokeSupport
import Test.Hspec

spec :: Spec
spec = describe "HTTP organization deletion" $ it "checks confirmation, projection isolation, recreation and monotonic reseeding" $ withFreshServer $ \client -> do
  initial <- get client "dashboard"
  field initial "organization" `shouldBe` Null
  field initial "version" `shouldBe` Number 0
  _ <- post client "demo" empty 201
  state <- get client "dashboard"
  let org = field state "organization"
      identifier = case field org "id" of String text -> T.unpack text; value -> error (show value)
      version = number (field state "version")
      name = field org "name"
      delete target confirmation revision status = void (call client "DELETE" ("organizations/" <> target) (Just (object ["confirmName" .= confirmation, "expectedVersion" .= revision])) status)
  oldEvents <- get client "events"
  oldPeople <- get client "people"
  delete "missing" name version 404
  delete identifier (String "wrong") version 400
  delete identifier name (version - 1) 409
  _ <- call client "DELETE" ("organizations/" <> identifier) (Just (object ["confirmName" .= name])) 400
  get client "events" `shouldReturn` oldEvents
  _ <- post client "people" (object ["id" .= String "deletion-test", "name" .= String "삭제 검증 가상 인물", "role" .= String "검증"]) 201
  delete identifier name version 409
  currentVersion <- number . (`field` "version") <$> get client "dashboard"
  delete identifier name currentVersion 200
  cleared <- get client "dashboard"
  number (field cleared "version") `shouldBe` currentVersion + 1
  field cleared "organization" `shouldBe` Null
  mapM_ (\key -> items (field cleared key) `shouldBe` []) ["people", "goals", "authorities", "reviews"]
  mapM_ (\path -> (items <$> get client path) `shouldReturn` []) ["people", "goals", "events", "reviews"]
  graph <- get client "graph"
  items (field graph "nodes") `shouldBe` []
  items (field graph "edges") `shouldBe` []
  get client "organization" `shouldReturn` Null
  delete identifier name (number (field cleared "version")) 404
  _ <- post client "organizations" (object ["id" .= field org "id", "name" .= name]) 201
  new <- get client "dashboard"
  field new "demo" `shouldBe` Bool False
  mapM_ (\key -> items (field new key) `shouldBe` []) ["people", "goals", "reviews"]
  events <- items <$> get client "events"
  length events `shouldBe` 1
  number (field (first events) "seq") `shouldBe` number (field cleared "version") + 1
  delete identifier name currentVersion 409
  delete identifier name (number (field new "version")) 200
  _ <- post client "demo" empty 201
  seeded <- get client "dashboard"
  field seeded "demo" `shouldBe` Bool True
  length (items (field seeded "goals")) `shouldBe` 7
  reseededEvents <- items <$> get client "events"
  number (field (first reseededEvents) "seq") `shouldSatisfy` (> number (field new "version"))
  get client "people" `shouldReturn` oldPeople
