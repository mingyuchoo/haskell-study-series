{-# LANGUAGE OverloadedStrings #-}

module Interface.Web.Controller.UrlController
    ( createUrlHandler
    , homeHandler
    , redirectHandler
    ) where

import           Application.UseCase.ListUrls                    (listUrls)
import           Application.UseCase.RetrieveUrl                 (retrieveUrl)
import           Application.UseCase.ShortenUrl                  (shortenUrl)

import           Control.Monad.IO.Class                          (liftIO)

import           Data.IORef                                      (IORef)
import           Data.Map                                        (Map)
import qualified Data.Text.Lazy                                  as LT

import           Domain.Entity.Url                               (Url (..),
                                                                  mkUrl)

import           Infrastructure.Repository.InMemoryUrlRepository (runInMemoryUrlRepo)

import           Interface.Web.View.UrlView                      (renderHomePage)

import           Network.HTTP.Types                              (status400,
                                                                  status404)

import           Web.Scotty

type UrlStore = (Int, Map Int Url)

homeHandler :: IORef UrlStore -> ActionM ()
homeHandler storeRef = do
    urls <- liftIO $ runInMemoryUrlRepo listUrls storeRef
    html $ renderHomePage urls

createUrlHandler :: IORef UrlStore -> ActionM ()
createUrlHandler storeRef = do
    urlText <- formParam "url"
    case mkUrl urlText of
        Nothing -> raiseStatus status400 "Invalid URL"
        Just url -> do
            _ <- liftIO $ runInMemoryUrlRepo (shortenUrl url) storeRef
            redirect "/"

redirectHandler :: IORef UrlStore -> ActionM ()
redirectHandler storeRef = do
    urlId <- pathParam "n"
    maybeUrl <- liftIO $ runInMemoryUrlRepo (retrieveUrl urlId) storeRef
    case maybeUrl of
        Just url -> redirect (LT.fromStrict $ getUrl url)
        Nothing  -> raiseStatus status404 "not found"
