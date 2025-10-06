{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Entities.Article
    ( Article(..)
    , ArticleId
    , ArticleTitle(..)
    , ArticleBody(..)
    , mkArticle
    , validateArticle
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Int (Int64)
import GHC.Generics (Generic)
import Domain.Entities.User (UserId)

-- Domain types
newtype ArticleId = ArticleId Int64 deriving (Show, Eq, Generic)
newtype ArticleTitle = ArticleTitle Text deriving (Show, Eq, Generic)
newtype ArticleBody = ArticleBody Text deriving (Show, Eq, Generic)

-- Core domain entity
data Article = Article
    { articleId :: Maybe ArticleId
    , articleTitle :: ArticleTitle
    , articleBody :: ArticleBody
    , articlePublishedTime :: UTCTime
    , articleAuthorId :: UserId
    } deriving (Show, Eq, Generic)

-- Smart constructor with validation
mkArticle :: Text -> Text -> UTCTime -> UserId -> Either Text Article
mkArticle title body publishedTime authorId
    | T.null title = Left "Title cannot be empty"
    | T.null body = Left "Body cannot be empty"
    | otherwise = Right $ Article
        { articleId = Nothing
        , articleTitle = ArticleTitle title
        , articleBody = ArticleBody body
        , articlePublishedTime = publishedTime
        , articleAuthorId = authorId
        }

-- Domain validation
validateArticle :: Article -> Either Text Article
validateArticle article@(Article _ (ArticleTitle title) (ArticleBody body) _ _)
    | T.null title = Left "Title cannot be empty"
    | T.null body = Left "Body cannot be empty"
    | otherwise = Right article