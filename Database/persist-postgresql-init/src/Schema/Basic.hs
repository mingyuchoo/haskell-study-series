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
import           Database.Persist.Sql (fromSqlKey)
import qualified Database.Persist.TH as PTH

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User sql=users
    name Text
    email Text
    age Int
    occupation Text
    UniqueEmail email
    deriving Show Read
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
