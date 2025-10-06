{-# LANGUAGE FlexibleContexts #-}

module UseCases.User.GetUser
    ( GetUserUseCase(..)
    , GetUserRequest(..)
    , GetUserResponse(..)
    , getUser
    , getUserWithCache
    ) where

import Domain.Entities.User
import Domain.Repositories.UserRepository
import Domain.Services.CacheService
import Data.Text (Text)

-- Use case input/output DTOs
data GetUserRequest = GetUserRequest
    { grUserId :: UserId
    } deriving (Show, Eq)

data GetUserResponse = GetUserResponse
    { grUser :: User
    } deriving (Show, Eq)

-- Use case interface
class Monad m => GetUserUseCase m where
    executeGetUser :: GetUserRequest -> m (Either Text GetUserResponse)

-- Basic implementation without cache
getUser :: (UserRepository m) => GetUserRequest -> m (Either Text GetUserResponse)
getUser req = do
    maybeUser <- findUserById (grUserId req)
    case maybeUser of
        Nothing -> return $ Left "User not found"
        Just user -> return $ Right $ GetUserResponse user

-- Implementation with cache
getUserWithCache :: (UserRepository m, CacheService m) => GetUserRequest -> m (Either Text GetUserResponse)
getUserWithCache req = do
    let userId = grUserId req
    -- Try cache first
    cachedUser <- getCachedUser userId
    case cachedUser of
        Just user -> return $ Right $ GetUserResponse user
        Nothing -> do
            -- Cache miss, get from repository
            maybeUser <- findUserById userId
            case maybeUser of
                Nothing -> return $ Left "User not found"
                Just user -> do
                    -- Cache the result
                    cacheUser userId user
                    return $ Right $ GetUserResponse user