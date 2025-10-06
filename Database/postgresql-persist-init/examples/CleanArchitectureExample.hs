{-# LANGUAGE OverloadedStrings #-}

module CleanArchitectureExample where

import Domain.Entities.User
import UseCases.User.CreateUser
import UseCases.User.GetUser
import Infrastructure.Persistence.PostgreSQL.UserRepositoryImpl
import Infrastructure.Cache.Redis.CacheServiceImpl
import Application.UserService

-- Example: Creating a user with the new Clean Architecture

-- 1. Pure domain logic - no dependencies
exampleCreateUserDomain :: Either Text User
exampleCreateUserDomain = mkUser "John Doe" "john@example.com" 30 "Developer"

-- 2. Use case with repository dependency
exampleCreateUserUseCase :: IO (Either Text CreateUserResponse)
exampleCreateUserUseCase = do
    let request = CreateUserRequest "John Doe" "john@example.com" 30 "Developer"
    
    -- Run with PostgreSQL repository
    runPostgreSQLUserRepository localConnString $ createUser request

-- 3. Full application service with both repository and cache
exampleCreateUserWithCache :: IO (Either Text CreateUserResponseDTO)
exampleCreateUserWithCache = do
    let request = CreateUserRequestDTO "John Doe" "john@example.com" 30 "Developer"
    
    -- Run with both PostgreSQL and Redis
    runCachedUserService 
        (PostgreSQLUserRepository undefined) -- Properly initialize in real code
        (RedisCacheService undefined)        -- Properly initialize in real code
        (handleCreateUser request)

-- Example: Benefits of Clean Architecture

-- 1. Easy to test business logic in isolation
testUserValidation :: Bool
testUserValidation = 
    case mkUser "" "invalid" (-1) "" of
        Left _ -> True   -- Validation correctly failed
        Right _ -> False -- Should not succeed

-- 2. Easy to swap implementations
-- Can easily switch from PostgreSQL to MongoDB by implementing UserRepository
-- Can easily switch from Redis to Memcached by implementing CacheService

-- 3. Easy to add new use cases
-- New business requirements only require changes to Domain and UseCases layers
-- Infrastructure and Interface layers remain unchanged

-- 4. Clear dependency direction
-- Domain ← UseCases ← Application ← Infrastructure
-- No circular dependencies, easy to understand and maintain