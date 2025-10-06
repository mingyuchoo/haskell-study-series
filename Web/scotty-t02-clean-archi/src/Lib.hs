{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( shortener
    ) where

import           Data.IORef                             (newIORef)
import           Data.Map                               (Map)
import qualified Data.Map                               as M

import           Domain.Entity.Url                      (Url)

import           Adapters.Web.Controller.UrlController (createUrlHandler,
                                                         homeHandler,
                                                         redirectHandler)

import           Web.Scotty

-- | shortener
--
--
shortener :: IO ()
shortener = do
    urlsR <- newIORef (1 :: Int, mempty :: Map Int Url)
    scotty 8000 $ do
        get "/" $ homeHandler urlsR
        post "/" $ createUrlHandler urlsR
        get "/:n" $ redirectHandler urlsR

