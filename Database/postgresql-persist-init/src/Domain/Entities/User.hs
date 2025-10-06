{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Entities.User
    ( User(..)
    , UserId(..)
    , UserEmail(..)
    , UserName(..)
    , UserAge(..)
    , UserOccupation(..)
    , mkUser
    , validateUser
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Int (Int64)
import GHC.Generics (Generic)

-- Domain types with validation
newtype UserId = UserId Int64 deriving (Show, Eq, Read, Generic)
newtype UserEmail = UserEmail Text deriving (Show, Eq, Read, Generic)
newtype UserName = UserName Text deriving (Show, Eq, Read, Generic)
newtype UserAge = UserAge Int deriving (Show, Eq, Read, Generic)
newtype UserOccupation = UserOccupation Text deriving (Show, Eq, Read, Generic)

-- Core domain entity
data User = User
    { userId :: Maybe UserId
    , userName :: UserName
    , userEmail :: UserEmail
    , userAge :: UserAge
    , userOccupation :: UserOccupation
    } deriving (Show, Eq, Read, Generic)

-- Smart constructor with validation
mkUser :: Text -> Text -> Int -> Text -> Either Text User
mkUser name email age occupation
    | T.null name = Left "Name cannot be empty"
    | T.null email = Left "Email cannot be empty"
    | age < 0 = Left "Age cannot be negative"
    | age > 150 = Left "Age cannot exceed 150"
    | T.null occupation = Left "Occupation cannot be empty"
    | otherwise = Right $ User
        { userId = Nothing
        , userName = UserName name
        , userEmail = UserEmail email
        , userAge = UserAge age
        , userOccupation = UserOccupation occupation
        }

-- Domain validation
validateUser :: User -> Either Text User
validateUser user@(User _ (UserName name) (UserEmail email) (UserAge age) (UserOccupation occupation))
    | T.null name = Left "Name cannot be empty"
    | T.null email = Left "Email cannot be empty"
    | age < 0 = Left "Age cannot be negative"
    | age > 150 = Left "Age cannot exceed 150"
    | T.null occupation = Left "Occupation cannot be empty"
    | otherwise = Right user