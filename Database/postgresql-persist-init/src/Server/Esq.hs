{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server.Esq
    where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Control.Exception          (try)
import           Data.Int                   (Int64)
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text)
import           Database.Persist           (Entity)
import           Database.PostgreSQL.Simple (SqlError(..))
import           DB.Esq
import           Network.Wai.Handler.Warp   (run)
import           Schema.Esq
import           Servant.API
import           Servant.Client
import           Servant.Server

type FullAPI =
       Get '[JSON] [Text]
  :<|> "users" :> Capture "userid" Int64 :> Get '[JSON] User
  :<|> "users" :> Capture "userid" Int64 :> ReqBody '[JSON] User :> Put '[JSON] NoContent
  :<|> "users" :> Capture "userid" Int64 :> ReqBody '[JSON] UserPatch :> Patch '[JSON] NoContent
  :<|> "users" :> Capture "userid" Int64 :> Delete '[JSON] NoContent
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64
  :<|> "users" :> Get '[JSON] [Entity User]
  :<|> "articles" :> Capture "articleid" Int64 :> Get '[JSON] Article
  :<|> "articles" :> ReqBody '[JSON] Article :> Post '[JSON] Int64
  :<|> "articles" :> "author" :> Capture "authorid" Int64 :> Get '[JSON] [Entity Article]
  :<|> "articles" :> "recent" :> Get '[JSON] [(Entity User, Entity Article)]

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
fetchUsersHandler :: PGInfo -> Int64 -> Handler User
fetchUsersHandler pgInfo uid = do
  maybeUser <- liftIO $ fetchUserPG pgInfo uid
  case maybeUser of
    Just user -> return user
    Nothing -> Handler $ throwE (err401 { errBody = "Could not find user with that ID" })

replaceUserHandler :: PGInfo -> Int64 -> User -> Handler NoContent
replaceUserHandler pgInfo uid user = do
  replaced <- liftIO $ replaceUserPG pgInfo uid user
  let result
        | replaced  = pure NoContent
        | otherwise = Handler $ throwE (err404 { errBody = "User not found" })
  result

patchUserHandler :: PGInfo -> Int64 -> UserPatch -> Handler NoContent
patchUserHandler pgInfo uid patchBody = do
  updated <- liftIO $ patchUserPG pgInfo uid patchBody
  let result
        | updated   = pure NoContent
        | otherwise = Handler $ throwE (err404 { errBody = "User not found" })
  result

createUserHandler :: PGInfo -> User -> Handler Int64
createUserHandler pgInfo user = do
  result <- liftIO $ (try (createUserPG pgInfo user) :: IO (Either SqlError Int64))
  case result of
    Right newId -> pure newId
    Left e
      | sqlState e == "23505" -> Handler $ throwE (err409 { errBody = "Email already exists" })
      | otherwise              -> Handler $ throwE (err500 { errBody = "Database error" })

listUsersHandler :: PGInfo -> Handler [Entity User]
listUsersHandler pgInfo = liftIO $ fetchAllUsersPG pgInfo

deleteUserHandler :: PGInfo -> Int64 -> Handler NoContent
deleteUserHandler pgInfo uid = do
  mUser <- liftIO $ fetchUserPG pgInfo uid
  case mUser of
    Nothing   -> Handler $ throwE $ err404 { errBody = "User not found" }
    Just _usr -> liftIO (deleteUserPG pgInfo uid) >> pure NoContent

fetchArticleHandler :: PGInfo -> Int64 -> Handler Article
fetchArticleHandler pgInfo aid = do
  maybeArticle <- liftIO $ fetchArticlePG pgInfo aid
  case maybeArticle of
    Just article -> return article
    Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find article with that ID" })

createArticleHandler :: PGInfo -> Article -> Handler Int64
createArticleHandler pgInfo article = liftIO $ createArticlePG pgInfo article

fetchArticlesByAuthorHandler :: PGInfo -> Int64 -> Handler [Entity Article]
fetchArticlesByAuthorHandler pgInfo uid = liftIO $ fetchArticlesByAuthorPG pgInfo uid

fetchRecentArticlesHandler :: PGInfo -> Handler [(Entity User, Entity Article)]
fetchRecentArticlesHandler pgInfo = liftIO $ fetchRecentArticlesPG pgInfo

fullAPIServer :: PGInfo -> Server FullAPI
fullAPIServer pgInfo =
  rootApiListHandler :<|>
  (fetchUsersHandler pgInfo) :<|>
  (replaceUserHandler pgInfo) :<|>
  (patchUserHandler pgInfo) :<|>
  (deleteUserHandler pgInfo) :<|>
  (createUserHandler pgInfo) :<|>
  (listUsersHandler pgInfo) :<|>
  (fetchArticleHandler pgInfo) :<|>
  (createArticleHandler pgInfo) :<|>
  (fetchArticlesByAuthorHandler pgInfo) :<|>
  (fetchRecentArticlesHandler pgInfo)

runServer :: IO ()
runServer = run 8000 (serve usersAPI (fullAPIServer localConnString))

rootApiListClient :: ClientM [Text]
fetchUserClient :: Int64 -> ClientM User
replaceUserClient :: Int64 -> User -> ClientM NoContent
patchUserClient :: Int64 -> UserPatch -> ClientM NoContent
deleteUserClient :: Int64 -> ClientM NoContent
createUserClient :: User -> ClientM Int64
listUsersClient :: ClientM [Entity User]
fetchArticleClient :: Int64 -> ClientM Article
createArticleClient :: Article -> ClientM Int64
fetchArticlesByAuthorClient :: Int64 -> ClientM [Entity Article]
fetchRecentArticlesClient :: ClientM [(Entity User, Entity Article)]
( rootApiListClient           :<|>
  fetchUserClient             :<|>
  replaceUserClient           :<|>
  patchUserClient             :<|>
  deleteUserClient            :<|>
  createUserClient            :<|>
  listUsersClient             :<|>
  fetchArticleClient          :<|>
  createArticleClient         :<|>
  fetchArticlesByAuthorClient :<|>
  fetchRecentArticlesClient )  = client (Proxy :: Proxy FullAPI)
