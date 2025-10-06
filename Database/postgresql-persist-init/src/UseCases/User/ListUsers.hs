{-# LANGUAGE FlexibleContexts #-}

module UseCases.User.ListUsers
    ( ListUsersUseCase(..)
    , ListUsersRequest(..)
    , ListUsersResponse(..)
    , listUsers
    ) where

import Domain.Entities.User
import Domain.Repositories.UserRepository
import Data.Text (Text)

-- Use case input/output DTOs
data ListUsersRequest = ListUsersRequest
    deriving (Show, Eq)

data ListUsersResponse = ListUsersResponse
    { lrUsers :: [User]
    } deriving (Show, Eq)

-- Use case interface
class Monad m => ListUsersUseCase m where
    executeListUsers :: ListUsersRequest -> m (Either Text ListUsersResponse)

-- Use case implementation
listUsers :: (UserRepository m) => ListUsersRequest -> m (Either Text ListUsersResponse)
listUsers _ = do
    users <- findAllUsers
    return $ Right $ ListUsersResponse users