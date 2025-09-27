{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server.Cache
    where

import           Schema.Basic
import           Schema.Cache               (RedisInfo, localRedisInfo)
import           DB.Cache
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Except  (throwE)
import           Data.Int                    (Int64)
import           Data.Proxy                  (Proxy (..))
import           Data.Text                   (Text)
import           DB.Basic                    (PGInfo, createUserPG,
                                              fetchAllUsersPG, fetchUserPG,
                                              putUserPG, patchUserPG, deleteUserPG,
                                              localConnString, migrateDB)
import           Database.Persist            (Entity)
import           Network.Wai.Handler.Warp    (run)
import           Servant.API
import           Servant.Server

-- API matches Server.Basic

type FullAPI =
       Get '[JSON] [Text]
  :<|> "users" :> Capture "userid" Int64 :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64
  :<|> "users" :> Get '[JSON] [Entity User]
  :<|> "users" :> Capture "userid" Int64 :> ReqBody '[JSON] User        :> PutNoContent
  :<|> "users" :> Capture "userid" Int64 :> ReqBody '[JSON] UpdateUser  :> PatchNoContent
  :<|> "users" :> Capture "userid" Int64                                  :> DeleteNoContent

usersAPI :: Proxy FullAPI
usersAPI = Proxy :: Proxy FullAPI

rootApiListHandler :: Handler [Text]
rootApiListHandler = pure
  [ "/"
  , "POST /users"
  , "GET /users"
  , "GET /users/{id}"
  , "PUT /users/{id}"
  , "PATCH /users/{id}"
  , "DELETE /users/{id}"
  ]

-- Redis-backed fetch for a user by id
fetchUsersHandler :: PGInfo -> RedisInfo -> Int64 -> Handler User
fetchUsersHandler pgInfo redisInfo uid = do
  maybeCachedUser <- liftIO $ fetchUserRedis redisInfo uid
  case maybeCachedUser of
    Just user -> return user
    Nothing -> do
      maybeUser <- liftIO $ fetchUserPG pgInfo uid
      case maybeUser of
        Just user -> liftIO (cacheUser redisInfo uid user) >> return user
        Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find user with that ID" })

createUserHandler :: PGInfo -> User -> Handler Int64
createUserHandler pgInfo user = liftIO $ createUserPG pgInfo user

listUsersHandler :: PGInfo -> Handler [Entity User]
listUsersHandler pgInfo = liftIO $ fetchAllUsersPG pgInfo

-- Replace full user (PUT). Update DB and refresh cache with new value
putUserHandler :: PGInfo -> RedisInfo -> Int64 -> User -> Handler NoContent
putUserHandler pgInfo redisInfo uid user = do
  liftIO $ putUserPG pgInfo uid user
  -- refresh cache to the latest value
  liftIO $ cacheUser redisInfo uid user
  pure NoContent

-- Partial update (PATCH). Update DB and invalidate cache to avoid stale data
patchUserHandler :: PGInfo -> RedisInfo -> Int64 -> UpdateUser -> Handler NoContent
patchUserHandler pgInfo redisInfo uid update = do
  liftIO $ patchUserPG pgInfo uid update
  -- simplest approach: invalidate cache
  liftIO $ deleteUserCache redisInfo uid
  pure NoContent

-- Delete user (DELETE). Remove from DB and invalidate cache
deleteUserHandler :: PGInfo -> RedisInfo -> Int64 -> Handler NoContent
deleteUserHandler pgInfo redisInfo uid = do
  liftIO $ deleteUserPG pgInfo uid
  liftIO $ deleteUserCache redisInfo uid
  pure NoContent

usersServer :: PGInfo -> RedisInfo -> Server FullAPI
usersServer pgInfo redisInfo =
  rootApiListHandler :<|>
  (fetchUsersHandler pgInfo redisInfo) :<|>
  (createUserHandler pgInfo) :<|>
  (listUsersHandler pgInfo) :<|>
  (putUserHandler pgInfo redisInfo) :<|>
  (patchUserHandler pgInfo redisInfo) :<|>
  (deleteUserHandler pgInfo redisInfo)

runServer :: IO ()
runServer = do
  -- Ensure DB schema exists before serving
  migrateDB localConnString
  run 8000 (serve usersAPI (usersServer localConnString localRedisInfo))
