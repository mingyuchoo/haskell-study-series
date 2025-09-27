{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server.Cache
    where

import           Schema.Basic
import           DB.Cache
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Except  (throwE)
import           Data.Int                    (Int64)
import           Data.Proxy                  (Proxy (..))
import           Data.Text                   (Text)
import           DB.Basic                    (PGInfo, createUserPG,
                                              fetchAllUsersPG, fetchUserPG,
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

usersAPI :: Proxy FullAPI
usersAPI = Proxy :: Proxy FullAPI

rootApiListHandler :: Handler [Text]
rootApiListHandler = pure
  [ "/"
  , "POST /users"
  , "GET /users"
  , "GET /users/{id}"
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

usersServer :: PGInfo -> RedisInfo -> Server FullAPI
usersServer pgInfo redisInfo =
  rootApiListHandler :<|>
  (fetchUsersHandler pgInfo redisInfo) :<|>
  (createUserHandler pgInfo) :<|>
  (listUsersHandler pgInfo)

runServer :: IO ()
runServer = do
  -- Ensure DB schema exists before serving
  migrateDB localConnString
  run 8000 (serve usersAPI (usersServer localConnString localRedisInfo))
