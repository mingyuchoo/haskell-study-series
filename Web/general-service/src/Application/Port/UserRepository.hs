module Application.Port.UserRepository
  ( UserRepository (..)
  )
where

import Data.Text (Text)
import Domain.User (ProfileUpdate, User)

data UserRepository m = UserRepository
  { findUserByEmail :: Text -> m (Maybe User)
  , createStoredUser :: Text -> Text -> Text -> m User
  , findUserById :: Int -> m (Maybe User)
  , updateStoredProfile :: Int -> ProfileUpdate -> m (Maybe User)
  , createStoredSession :: Int -> Text -> m ()
  , findUserBySession :: Text -> m (Maybe User)
  , deleteStoredSession :: Text -> m ()
  }
