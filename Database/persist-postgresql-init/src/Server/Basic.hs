{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server.Basic
    where

import           Schema.Basic

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Except  (throwE)

import           Data.Int                    (Int64)
import           Data.Proxy                  (Proxy (..))
import           Data.Text                   (Text)

import           DB.Basic                    (PGInfo, createUserPG,
                                              fetchAllUsersPG, fetchUserPG,
                                              localConnString)
import           Database.Persist            (Entity)

import           Network.Wai.Handler.Warp    (run)

import           Servant.API
import           Servant.Server

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

fetchUsersHandler :: PGInfo -> Int64 -> Handler User
fetchUsersHandler connString uid = do
  maybeUser <- liftIO $ fetchUserPG connString uid
  case maybeUser of
    Just user -> return user
    Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find user with that ID" })

createUserHandler :: PGInfo -> User -> Handler Int64
createUserHandler connString user = liftIO $ createUserPG connString user

listUsersHandler :: PGInfo -> Handler [Entity User]
listUsersHandler connString = liftIO $ fetchAllUsersPG connString

usersServer :: PGInfo -> Server FullAPI
usersServer connString =
  rootApiListHandler :<|>
  (fetchUsersHandler connString) :<|>
  (createUserHandler connString) :<|>
  (listUsersHandler connString)

runServer :: IO ()
runServer = run 8000 (serve usersAPI (usersServer localConnString))
