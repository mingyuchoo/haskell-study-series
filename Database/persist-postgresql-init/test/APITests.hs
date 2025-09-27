module Main
    where

import           Control.Concurrent (killThread)

import           DB.Basic
import           DB.Cache
import           Schema.Cache       (RedisInfo)

import           Data.Either        (isLeft)
import           Data.Int           (Int64)
import           Data.Maybe         (isJust)
import           Data.List          (find)

import           Schema.Basic       (User (..), UpdateUser (..))

import           Servant.Client     (ClientEnv, runClientM)
import           Database.Persist   (Entity (..))

import           Server.Cache       ( createUserClient
                                   , fetchUserClient
                                   , listUsersClient
                                   , putUserClient
                                   , patchUserClient
                                   , deleteUserClient
                                   )

import           Test.Hspec

import           TestUtils          (setupTests)

main :: IO ()
main = do
  (pgInfo, redisInfo, clientEnv, tid) <- setupTests
  hspec $ before (beforeHook1 clientEnv pgInfo redisInfo) spec1
  hspec $ before (beforeHook2 clientEnv pgInfo redisInfo) $ after (afterHook pgInfo redisInfo) $ spec2
  hspec $ before (beforeHook3 clientEnv pgInfo redisInfo) $ after (afterHook pgInfo redisInfo) $ spec3
  hspec $ before (beforeHookList clientEnv pgInfo) $ after (afterHookList pgInfo) $ specList
  hspec $ before (beforeHookPut clientEnv pgInfo redisInfo) $ after (afterHookPut pgInfo redisInfo) $ specPut
  hspec $ before (beforeHookPatch clientEnv pgInfo redisInfo) $ after (afterHookPatch pgInfo redisInfo) $ specPatch
  hspec $ before (beforeHookDelete clientEnv pgInfo redisInfo) $ specDelete
  killThread tid
  return ()

beforeHook1 :: ClientEnv -> PGInfo -> RedisInfo -> IO (Bool, Bool, Bool)
beforeHook1 clientEnv pgInfo redisInfo = do
  callResult <- runClientM (fetchUserClient 1) clientEnv
  let throwsError = isLeft (callResult)
  inPG <- isJust <$> fetchUserPG pgInfo 1
  inRedis <- isJust <$> fetchUserRedis redisInfo 1
  return (throwsError, inPG, inRedis)

spec1 :: SpecWith (Bool, Bool, Bool)
spec1 = describe "After fetching on an empty database" $ do
  it "The fetch call should throw an error" $ \(throwsError, _, _) -> throwsError `shouldBe` True
  it "There should be no user in Postgres" $ \(_, inPG, _) -> inPG `shouldBe` False
  it "There should be no user in Redis" $ \(_, _, inRedis) -> inRedis `shouldBe` False

beforeHook2 :: ClientEnv -> PGInfo -> RedisInfo -> IO (Bool, Bool, Int64)
beforeHook2 clientEnv pgInfo redisInfo = do
  userKeyEither <- runClientM (createUserClient testUser) clientEnv
  case userKeyEither of
    Left _ -> error "DB call failed on spec 2!"
    Right userKey -> do
      inPG <- isJust <$> fetchUserPG pgInfo userKey
      inRedis <- isJust <$> fetchUserRedis redisInfo userKey
      return (inPG, inRedis, userKey)

spec2 :: SpecWith (Bool, Bool, Int64)
spec2 = describe "After creating the user but not fetching" $ do
  it "There should be a user in Postgres" $ \(inPG, _, _) -> inPG `shouldBe` True
  it "There should be no user in Redis" $ \(_, inRedis, _) -> inRedis `shouldBe` False

afterHook :: PGInfo -> RedisInfo -> (Bool, Bool, Int64) -> IO ()
afterHook pgInfo redisInfo (_, _, key) = do
  deleteUserCache redisInfo key
  deleteUserPG pgInfo key

-- Cleanup for PUT spec tuple (User, User, Int64, Bool, Bool)
afterHookPut :: PGInfo -> RedisInfo -> (User, User, Int64, Bool, Bool) -> IO ()
afterHookPut pgInfo redisInfo (_, _, key, _, _) = do
  deleteUserCache redisInfo key
  deleteUserPG pgInfo key

beforeHook3 :: ClientEnv -> PGInfo -> RedisInfo -> IO (Bool, Bool, Int64)
beforeHook3 clientEnv pgInfo redisInfo = do
  userKeyEither <- runClientM (createUserClient testUser) clientEnv
  case userKeyEither of
    Left _ -> error "DB call failed on spec 3!"
    Right userKey -> do
      _ <- runClientM (fetchUserClient userKey) clientEnv
      inPG <- isJust <$> fetchUserPG pgInfo userKey
      inRedis <- isJust <$> fetchUserRedis redisInfo userKey
      return (inPG, inRedis, userKey)

spec3 :: SpecWith (Bool, Bool, Int64)
spec3 = describe "After creating the user and fetching" $ do
  it "There should be a user in Postgres" $ \(inPG, _, _) -> inPG `shouldBe` True
  it "There should be a user in Redis" $ \(_, inRedis, _) -> inRedis `shouldBe` True

-- List users after creating two users
beforeHookList :: ClientEnv -> PGInfo -> IO ([User], [Int64])
beforeHookList clientEnv pgInfo = do
  e1 <- runClientM (createUserClient testUser) clientEnv
  e2 <- runClientM (createUserClient testUser2) clientEnv
  case (e1, e2) of
    (Right u1, Right u2) -> do
      ents <- runClientM listUsersClient clientEnv
      case ents of
        Right es -> do
          let users = map (\(Entity _ u) -> u) es
          return (users, [u1, u2])
        Left _ -> error "List users failed"
    _ -> error "Create users failed for list test"

afterHookList :: PGInfo -> ([User], [Int64]) -> IO ()
afterHookList pgInfo (_, keys) = mapM_ (deleteUserPG pgInfo) keys

specList :: SpecWith ([User], [Int64])
specList = describe "List users after multiple creations" $ do
  it "Response should contain both created users by email" $ \(users, _) -> do
    (isJust $ find ((== userEmail testUser) . userEmail) users) `shouldBe` True
    (isJust $ find ((== userEmail testUser2) . userEmail) users) `shouldBe` True

-- PUT should replace DB and refresh cache
beforeHookPut :: ClientEnv -> PGInfo -> RedisInfo -> IO (User, User, Int64, Bool, Bool)
beforeHookPut clientEnv pgInfo redisInfo = do
  Right key <- runClientM (createUserClient testUser) clientEnv
  -- perform PUT with replacement user
  _ <- runClientM (putUserClient key testUserRepl) clientEnv
  -- fetch via API to also ensure cache updated
  _ <- runClientM (fetchUserClient key) clientEnv
  inPG <- isJust <$> fetchUserPG pgInfo key
  inRedis <- isJust <$> fetchUserRedis redisInfo key
  return (testUser, testUserRepl, key, inPG, inRedis)

specPut :: SpecWith (User, User, Int64, Bool, Bool)
specPut = describe "PUT replaces user and refreshes cache" $ do
  it "User exists in Postgres after PUT" $ \(_, _, _, inPG, _) -> inPG `shouldBe` True
  it "User exists in Redis after PUT+fetch" $ \(_, _, _, _, inRedis) -> inRedis `shouldBe` True
  it "Fields are replaced (name and occupation)" $ \(_, _, key, _, _) -> do
    let assertUpdated u = do
          userName u `shouldBe` userName testUserRepl
          userOccupation u `shouldBe` userOccupation testUserRepl
    -- check DB
    mDb <- fetchUserPG localConnString key
    maybe (expectationFailure "No user in DB") assertUpdated mDb

-- PATCH should update partial fields and invalidate cache
beforeHookPatch :: ClientEnv -> PGInfo -> RedisInfo -> IO (PGInfo, RedisInfo, ClientEnv, Int64)
beforeHookPatch clientEnv pgInfo redisInfo = do
  Right key <- runClientM (createUserClient testUser) clientEnv
  -- warm cache by fetching once
  _ <- runClientM (fetchUserClient key) clientEnv
  -- patch only age
  let patchBody = UpdateUser { uuName = Nothing, uuEmail = Nothing, uuAge = Just 30, uuOccupation = Nothing }
  _ <- runClientM (patchUserClient key patchBody) clientEnv
  return (pgInfo, redisInfo, clientEnv, key)

specPatch :: SpecWith (PGInfo, RedisInfo, ClientEnv, Int64)
specPatch = describe "PATCH updates partial fields and invalidates cache" $ do
  it "After PATCH, fetching returns updated age" $ \(pgInfo, redisInfo, clientEnv, key) -> do
    -- cache should be invalidated, so redis miss
    r <- fetchUserRedis redisInfo key
    r `shouldBe` Nothing
    -- API fetch should return updated value and repopulate cache
    Right user <- runClientM (fetchUserClient key) clientEnv
    userAge user `shouldBe` 30
    -- cache now warm
    isJust <$> fetchUserRedis redisInfo key >>= (`shouldBe` True)

-- Cleanup after PATCH spec to avoid unique constraint conflicts in subsequent tests
afterHookPatch :: PGInfo -> RedisInfo -> (PGInfo, RedisInfo, ClientEnv, Int64) -> IO ()
afterHookPatch pgInfo redisInfo (_, _, _, key) = do
  deleteUserCache redisInfo key
  deleteUserPG pgInfo key

-- DELETE should remove from DB and Redis, and subsequent fetch should error
beforeHookDelete :: ClientEnv -> PGInfo -> RedisInfo -> IO (PGInfo, RedisInfo, ClientEnv, Int64)
beforeHookDelete clientEnv pgInfo redisInfo = do
  -- Use a unique email to avoid UniqueEmail conflicts from prior specs
  Right key <- runClientM (createUserClient testUserDel) clientEnv
  -- warm cache then delete through API
  _ <- runClientM (fetchUserClient key) clientEnv
  _ <- runClientM (deleteUserClient key) clientEnv
  return (pgInfo, redisInfo, clientEnv, key)

specDelete :: SpecWith (PGInfo, RedisInfo, ClientEnv, Int64)
specDelete = describe "DELETE removes user and prevents subsequent fetch" $ do
  it "Fetching after delete should return error and caches are empty" $ \(pgInfo, redisInfo, clientEnv, key) -> do
    -- Redis empty
    mR <- fetchUserRedis redisInfo key
    mR `shouldBe` Nothing
    -- PG empty
    mP <- fetchUserPG pgInfo key
    mP `shouldBe` Nothing
    -- API fetch should fail
    e <- runClientM (fetchUserClient key) clientEnv
    isLeft e `shouldBe` True

testUser :: User
testUser = User
  { userName = "james"
  , userEmail = "james@test.com"
  , userAge = 25
  , userOccupation = "Software Engineer"
  }

testUser2 :: User
testUser2 = User
  { userName = "amy"
  , userEmail = "amy@test.com"
  , userAge = 22
  , userOccupation = "Designer"
  }

testUserRepl :: User
testUserRepl = User
  { userName = "james2"
  , userEmail = "james@test.com" -- keep email same to avoid unique constraint
  , userAge = 27
  , userOccupation = "Architect"
  }

-- Dedicated user for DELETE test to avoid unique conflicts
testUserDel :: User
testUserDel = User
  { userName = "del"
  , userEmail = "del+unique@test.com"
  , userAge = 20
  , userOccupation = "Ops"
  }

