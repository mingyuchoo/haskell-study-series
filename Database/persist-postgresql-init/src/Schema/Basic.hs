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

module Schema.Basic
    where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text           (Text)
import           Database.Persist    (Entity (..))
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import qualified Database.Persist.TH as PTH

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User sql=users
    name Text
    email Text
    age Int
    occupation Text
    UniqueEmail email
    deriving Show Read Eq
|]

instance ToJSON (Entity User) where
  toJSON (Entity uid user) = object
    [ "id" .= fromSqlKey uid
    , "name" .= userName user
    , "email" .= userEmail user
    , "age" .= userAge user
    , "occupation" .= userOccupation user
    ]

instance ToJSON User where
  toJSON user = object
    [ "name" .= userName user
    , "email" .= userEmail user
    , "age" .= userAge user
    , "occupation" .= userOccupation user
    ]

instance FromJSON User where
  parseJSON = withObject "User" parseUser

-- Needed for decoding list endpoints with Servant Client
instance FromJSON (Entity User) where
  parseJSON = withObject "User Entity" $ \o -> do
    user <- parseUser o
    uid  <- o .: "id"
    return $ Entity (toSqlKey uid) user

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

-- Partial update type for PATCH requests
data UpdateUser = UpdateUser
  { uuName       :: Maybe Text
  , uuEmail      :: Maybe Text
  , uuAge        :: Maybe Int
  , uuOccupation :: Maybe Text
  } deriving (Show, Read)

instance FromJSON UpdateUser where
  parseJSON = withObject "UpdateUser" $ \o -> do
    uuName       <- o .:? "name"
    uuEmail      <- o .:? "email"
    uuAge        <- o .:? "age"
    uuOccupation <- o .:? "occupation"
    return UpdateUser {..}

-- Needed to send PATCH request bodies from Servant Client
instance ToJSON UpdateUser where
  toJSON UpdateUser{..} = object $ concat
    [ maybe [] (pure . ("name" .=)) uuName
    , maybe [] (pure . ("email" .=)) uuEmail
    , maybe [] (pure . ("age" .=)) uuAge
    , maybe [] (pure . ("occupation" .=)) uuOccupation
    ]
