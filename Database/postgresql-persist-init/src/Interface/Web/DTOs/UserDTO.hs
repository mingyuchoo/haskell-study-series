{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Interface.Web.DTOs.UserDTO
    ( UserResponseDTO(..)
    , CreateUserRequestDTO(..)
    , CreateUserResponseDTO(..)
    , UpdateUserRequestDTO(..)
    , userToResponseDTO
    , createUserRequestToUseCase
    , updateUserRequestToUseCase
    ) where

import Domain.Entities.User
import qualified UseCases.User.CreateUser as UC
import qualified UseCases.User.UpdateUser as UU
import Data.Aeson
import Data.Text (Text)
import Data.Int (Int64)
import GHC.Generics (Generic)

-- Response DTOs
data UserResponseDTO = UserResponseDTO
    { userId :: Int64
    , userName :: Text
    , userEmail :: Text
    , userAge :: Int
    , userOccupation :: Text
    } deriving (Show, Eq, Generic)

instance ToJSON UserResponseDTO
instance FromJSON UserResponseDTO

-- Request DTOs
data CreateUserRequestDTO = CreateUserRequestDTO
    { crName :: Text
    , crEmail :: Text
    , crAge :: Int
    , crOccupation :: Text
    } deriving (Show, Eq, Generic)

instance ToJSON CreateUserRequestDTO
instance FromJSON CreateUserRequestDTO

data CreateUserResponseDTO = CreateUserResponseDTO
    { crUserId :: Int64
    } deriving (Show, Eq, Generic)

instance ToJSON CreateUserResponseDTO
instance FromJSON CreateUserResponseDTO

data UpdateUserRequestDTO = UpdateUserRequestDTO
    { urName :: Maybe Text
    , urEmail :: Maybe Text
    , urAge :: Maybe Int
    , urOccupation :: Maybe Text
    } deriving (Show, Eq, Generic)

instance ToJSON UpdateUserRequestDTO
instance FromJSON UpdateUserRequestDTO

-- Conversion functions
userToResponseDTO :: User -> UserResponseDTO
userToResponseDTO (User (Just (UserId uid)) (UserName name) (UserEmail email) (UserAge age) (UserOccupation occupation)) =
    UserResponseDTO uid name email age occupation
userToResponseDTO (User Nothing _ _ _ _) = 
    error "Cannot convert user without ID to response DTO"

createUserRequestToUseCase :: CreateUserRequestDTO -> UC.CreateUserRequest
createUserRequestToUseCase dto = UC.CreateUserRequest
    { UC.crName = crName dto
    , UC.crEmail = crEmail dto
    , UC.crAge = crAge dto
    , UC.crOccupation = crOccupation dto
    }

updateUserRequestToUseCase :: Int64 -> UpdateUserRequestDTO -> UU.UpdateUserRequest
updateUserRequestToUseCase uid dto = UU.UpdateUserRequest
    { UU.urUserId = UserId uid
    , UU.urName = urName dto
    , UU.urEmail = urEmail dto
    , UU.urAge = urAge dto
    , UU.urOccupation = urOccupation dto
    }