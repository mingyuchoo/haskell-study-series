{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Schema.Esq
    where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text             (Text)
import           Data.Time             (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import           GHC.Generics          (Generic)

import           Database.Persist      (Entity (..))
import           Database.Persist.Sql  (fromSqlKey, toSqlKey)
import qualified Database.Persist.TH   as PTH

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User sql=users
    name Text
    email Text
    age Int
    occupation Text
    UniqueEmail email
    deriving Show Read Eq

  Article sql=articles
    title Text
    body Text
    publishedTime UTCTime
    authorId UserId
    UniqueTitle title
    deriving Show Read Eq
|]

instance ToJSON (Entity User) where
  toJSON (Entity uid user) = object $
    "id" .= (fromSqlKey uid) : userPairs user

instance ToJSON User where
  toJSON user = object (userPairs user)

userPairs :: User -> [Pair]
userPairs user =
  [ "name" .= userName user
  , "email" .= userEmail user
  , "age" .= userAge user
  , "occupation" .= userOccupation user
  ]

instance FromJSON (Entity User) where
  parseJSON = withObject "User Entity" $ \o -> do
    user <- parseUser o
    uid <- o .: "id"
    return $ Entity (toSqlKey uid) user

instance FromJSON User where
  parseJSON = withObject "User" parseUser

parseUser :: Object -> Parser User
parseUser o = do
  uName <- o .: "name"
  uEmail <- o .: "email"
  uAge <- o .: "age"
  uOccupation <- o .: "occupation"
  return User
    { userName = uName
    , userEmail = uEmail
    , userAge = uAge
    , userOccupation = uOccupation
    }

instance ToJSON (Entity Article) where
  toJSON (Entity aid article) = object $
    "id" .= (fromSqlKey aid) : articlePairs article

instance ToJSON Article where
  toJSON article = object (articlePairs article)

articlePairs :: Article -> [Pair]
articlePairs article =
  [ "title" .= articleTitle article
  , "body" .= articleBody article
  , "publishedTime" .= utcTimeToPOSIXSeconds (articlePublishedTime article)
  , "authorId" .= fromSqlKey (articleAuthorId article)
  ]

instance FromJSON (Entity Article) where
  parseJSON = withObject "Article Entity" $ \o -> do
    article <- parseArticle o
    aid <- o .: "id"
    return $ Entity (toSqlKey aid) article

instance FromJSON Article where
  parseJSON = withObject "Article" parseArticle

parseArticle :: Object -> Parser Article
parseArticle o = do
  aTitle <- o .: "title"
  aBody <- o .: "body"
  aPublishedTime <- o .: "publishedTime"
  aAuthorId <- o .: "authorId"
  return Article
    { articleTitle = aTitle
    , articleBody = aBody
    , articlePublishedTime = posixSecondsToUTCTime aPublishedTime
    , articleAuthorId = toSqlKey aAuthorId
    }

-- Partial update payload for PATCH /users/{id}
data UserPatch = UserPatch
  { upName       :: Maybe Text
  , upEmail      :: Maybe Text
  , upAge        :: Maybe Int
  , upOccupation :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON UserPatch where
  parseJSON = withObject "UserPatch" $ \o ->
    UserPatch
      <$> o .:? "name"
      <*> o .:? "email"
      <*> o .:? "age"
      <*> o .:? "occupation"

instance ToJSON UserPatch where
  toJSON UserPatch{..} = object $
    concat
      [ maybe [] (pure . ("name" .=)) upName
      , maybe [] (pure . ("email" .=)) upEmail
      , maybe [] (pure . ("age" .=)) upAge
      , maybe [] (pure . ("occupation" .=)) upOccupation
      ]
