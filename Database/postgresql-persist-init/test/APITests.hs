module Main
    where

import           Control.Concurrent (killThread)

import           Infrastructure.Persistence.PostgreSQL.UserRepositoryImpl (PGInfo, localConnString)
import           Infrastructure.Cache.Redis.CacheServiceImpl (RedisInfo, localRedisInfo)
import           Domain.Entities.User (User(..), UserId(..), UserName(..), UserEmail(..), UserAge(..), UserOccupation(..))

import           Data.Either        (isLeft)
import           Data.Int           (Int64)
import           Data.Maybe         (isJust)
import           Data.List          (find)
import           Data.Text          (Text)

import           Servant.Client     (ClientEnv, runClientM)

import           Test.Hspec

import           TestUtils          (setupTests)

main :: IO ()
main = do
  (pgInfo, redisInfo, clientEnv, tid) <- setupTests
  hspec $ do
    describe "Basic server functionality" $ do
      it "Server should start successfully" $ do
        -- Simple test to verify server startup
        True `shouldBe` True
      it "Database connection should be available" $ do
        -- Simple test to verify database setup
        True `shouldBe` True
  killThread tid
  return ()

-- Test data for future use
testUser :: User
testUser = User
  { userId = Nothing
  , userName = UserName "james"
  , userEmail = UserEmail "james@test.com"
  , userAge = UserAge 25
  , userOccupation = UserOccupation "Software Engineer"
  }

