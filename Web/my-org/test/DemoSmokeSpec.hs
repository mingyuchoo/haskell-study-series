module DemoSmokeSpec
  ( spec
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Data.List (nub, sort)
import SmokeSupport
import Test.Hspec

spec :: Spec
spec = describe "HTTP demo initialization" $ it "atomically seeds once under four concurrent requests" $ withFreshServer $ \client -> do
  responses <-
    mapConcurrently (\_ -> request client "POST" "demo" (Just empty)) [1 .. 4 :: Int]
  sort (map fst responses) `shouldBe` [201, 409, 409, 409]
  state <- get client "dashboard"
  length (items (field state "people")) `shouldSatisfy` (>= 6)
  let goals = items (field state "goals")
      active = filter (\entry -> field entry "active" == Bool True) goals
  length goals `shouldSatisfy` (>= 7)
  sort (nub (map (\entry -> field (field entry "evaluation") "status") active))
    `shouldBe` sort (map String ["NoData", "OnTrack", "AtRisk", "OffTrack", "Achieved"])
  length goals - length active `shouldSatisfy` (>= 2)
  goals `shouldSatisfy` any (\entry -> field entry "owner" == Null)
  map (`field` "code") (items (field (field state "compiler") "diagnostics"))
    `shouldHave` map String ["O001", "O017", "O031", "O040"]
  map (`field` "kind") (items (field (field state "graph") "edges"))
    `shouldHave` map String ["Owns", "DependsOn", "Controls", "Measures"]
  let reviews = items (field state "reviews")
  length reviews `shouldSatisfy` (>= 2)
  reviews
    `shouldSatisfy` any
      ( \review ->
          not (null (items (field review "learnings")))
            && not (null (items (field review "decisions")))
      )
  events <- get client "events"
  length (items events) `shouldSatisfy` (> 20)
  map (number . (`field` "seq")) (items events) `shouldBe` [1 .. length (items events)]
  length (nub (map (`field` "at") (items events))) `shouldBe` 1
  _ <- post client "demo" empty 409
  get client "events" `shouldReturn` events
  mapM_ (get client) ["organization", "people", "goals", "compiler", "graph", "reviews"]
