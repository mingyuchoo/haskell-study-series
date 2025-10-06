{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Infrastructure.Web.Server
    ( FullAPI
    , runBasicServer
    , runCachedServer
    , runEsqueletoServer
    ) where

import           Control.Monad.Error.Class                                (throwError)

import           Data.Proxy                                               (Proxy (..))
import           Data.Text                                                (Text)

import           Interface.Web.Controllers.UserController

import           Network.Wai.Handler.Warp                                 (run)

import           Servant.API
import           Servant.Server

-- Full API definition
type FullAPI =
       Get '[JSON] [Text]
  :<|> UserAPI

fullAPI :: Proxy FullAPI
fullAPI = Proxy

-- Root handler
rootHandler :: Handler [Text]
rootHandler = return
    [ "/"
    , "GET /users"
    , "POST /users"
    , "GET /users/{id}"
    , "PUT /users/{id}"
    , "PATCH /users/{id}"
    , "DELETE /users/{id}"
    ]

-- Basic server (no cache) - simplified for now
basicServer :: Server FullAPI
basicServer = rootHandler :<|> notImplementedUserAPI

-- Cached server - simplified for now
cachedServer :: Server FullAPI
cachedServer = rootHandler :<|> notImplementedUserAPI

-- Placeholder implementation
notImplementedUserAPI :: Server UserAPI
notImplementedUserAPI =
    (\_ -> throwError err501) :<|>
    (\_ -> throwError err501) :<|>
    (throwError err501) :<|>
    (\_ _ -> throwError err501) :<|>
    (\_ _ -> throwError err501) :<|>
    (\_ -> throwError err501)

-- Server runners
runBasicServer :: IO ()
runBasicServer = do
    putStrLn "Starting Basic Server on port 8000..."
    run 8000 (serve fullAPI basicServer)

runCachedServer :: IO ()
runCachedServer = do
    putStrLn "Starting Cached Server on port 8000..."
    run 8000 (serve fullAPI cachedServer)

runEsqueletoServer :: IO ()
runEsqueletoServer = do
    putStrLn "Starting Esqueleto Server on port 8000..."
    -- TODO: Implement Esqueleto version
    run 8000 (serve fullAPI basicServer)
