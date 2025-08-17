{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module ServerEsq
    where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (throwE)

import           Data.Int                   (Int64)
import           Data.Proxy                 (Proxy (..))

import           Database.Persist           (Entity, Key)

import           DatabaseEsq

import           Network.Wai.Handler.Warp   (run)

import           SchemaEsq

import           Servant.API
import           Servant.Client
import           Servant.Server

type FullAPI =
       "users" :> Capture "userid" Int64 :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64
  :<|> "articles" :> Capture "articleid" Int64 :> Get '[JSON] Article
  :<|> "articles" :> ReqBody '[JSON] Article :> Post '[JSON] Int64
  :<|> "articles" :> "author" :> Capture "authorid" Int64 :> Get '[JSON] [Entity Article]
  :<|> "articles" :> "recent" :> Get '[JSON] [(Entity User, Entity Article)]

usersAPI :: Proxy FullAPI
usersAPI = Proxy :: Proxy FullAPI

fetchUsersHandler :: PGInfo -> Int64 -> Handler User
fetchUsersHandler pgInfo uid = do
  maybeUser <- liftIO $ fetchUserPG pgInfo uid
  case maybeUser of
    Just user -> return user
    Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find user with that ID" })

createUserHandler :: PGInfo -> User -> Handler Int64
createUserHandler pgInfo user = liftIO $ createUserPG pgInfo user

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
  (fetchUsersHandler pgInfo) :<|>
  (createUserHandler pgInfo) :<|>
  (fetchArticleHandler pgInfo) :<|>
  (createArticleHandler pgInfo) :<|>
  (fetchArticlesByAuthorHandler pgInfo) :<|>
  (fetchRecentArticlesHandler pgInfo)

runServer :: IO ()
runServer = run 8000 (serve usersAPI (fullAPIServer localConnString))

fetchUserClient :: Int64 -> ClientM User
createUserClient :: User -> ClientM Int64
fetchArticleClient :: Int64 -> ClientM Article
createArticleClient :: Article -> ClientM Int64
fetchArticlesByAuthorClient :: Int64 -> ClientM [Entity Article]
fetchRecentArticlesClient :: ClientM [(Entity User, Entity Article)]
( fetchUserClient             :<|>
  createUserClient            :<|>
  fetchArticleClient          :<|>
  createArticleClient         :<|>
  fetchArticlesByAuthorClient :<|>
  fetchRecentArticlesClient )  = client (Proxy :: Proxy FullAPI)
