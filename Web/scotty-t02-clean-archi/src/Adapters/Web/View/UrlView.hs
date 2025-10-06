{-# LANGUAGE OverloadedStrings #-}

module Adapters.Web.View.UrlView
    ( renderHomePage
    ) where

import           Data.Foldable                 (for_)
import           Data.Map                      (Map)
import qualified Data.Map                      as M
import qualified Data.Text.Lazy                as LT

import           Domain.Entity.Url             (Url (..), UrlId)

import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

renderHomePage :: Map UrlId Url -> LT.Text
renderHomePage urls = renderHtml $
    H.html $
        H.body $ do
            H.h1 "Shortener"
            H.form H.! A.method "post" H.! A.action "/" $ do
                H.input H.! A.type_ "text" H.! A.name "url"
                H.input H.! A.type_ "submit"
            H.table $
                for_ (M.toList urls) $ \(urlId, url) ->
                    H.tr $ do
                        H.td (H.toHtml urlId)
                        H.td (H.text $ getUrl url)
