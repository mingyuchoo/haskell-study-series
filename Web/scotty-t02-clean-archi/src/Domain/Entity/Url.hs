{-# LANGUAGE OverloadedStrings #-}

module Domain.Entity.Url
    ( Url (..)
    , UrlId
    , mkUrl
    ) where

import           Data.Text (Text)

type UrlId = Int

newtype Url = Url { getUrl :: Text }
     deriving (Eq, Show)

mkUrl :: Text -> Maybe Url
mkUrl text
    | text == "" = Nothing
    | otherwise  = Just (Url text)
