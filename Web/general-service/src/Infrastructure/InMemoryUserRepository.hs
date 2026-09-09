module Infrastructure.InMemoryUserRepository
  ( newInMemoryUserRepository
  )
where

import Application.Port.UserRepository (UserRepository (..))
import Control.Concurrent.MVar (modifyMVar, newMVar, readMVar)
import Data.List (find)
import Data.Text (Text)
import Domain.User (ProfileUpdate (..), User (..))

data Store = Store [User] [(Text, Int)]

newInMemoryUserRepository :: IO (UserRepository IO)
newInMemoryUserRepository = do
  store <- newMVar (Store [] [])
  pure
    UserRepository
      { findUserByEmail = \email -> do
          Store users _ <- readMVar store
          pure (find ((== email) . userEmail) users)
      , createStoredUser = \email displayName passwordHash ->
          modifyMVar store $ \(Store users sessions) ->
            let user = User (nextId users) email displayName passwordHash
             in pure (Store (users <> [user]) sessions, user)
      , findUserById = \identifier -> do
          Store users _ <- readMVar store
          pure (find ((== identifier) . userId) users)
      , updateStoredProfile = \identifier (ProfileUpdate displayName) ->
          modifyMVar store $ \(Store users sessions) ->
            case find ((== identifier) . userId) users of
              Nothing -> pure (Store users sessions, Nothing)
              Just user ->
                let updated = user {userDisplayName = displayName}
                 in pure (Store (map (replaceUser updated) users) sessions, Just updated)
      , createStoredSession = \identifier token ->
          modifyMVar store $ \(Store users sessions) ->
            pure (Store users ((token, identifier) : filter ((/= token) . fst) sessions), ())
      , findUserBySession = \token -> do
          Store users sessions <- readMVar store
          pure $ do
            identifier <- lookup token sessions
            find ((== identifier) . userId) users
      , deleteStoredSession = \token ->
          modifyMVar store $ \(Store users sessions) ->
            pure (Store users (filter ((/= token) . fst) sessions), ())
      }

nextId :: [User] -> Int
nextId [] = 1
nextId users = maximum (map userId users) + 1

replaceUser :: User -> User -> User
replaceUser updated current
  | userId updated == userId current = updated
  | otherwise = current
