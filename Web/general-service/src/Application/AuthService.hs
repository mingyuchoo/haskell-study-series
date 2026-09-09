{-# LANGUAGE OverloadedStrings #-}

module Application.AuthService
  ( Session (..)
  , signUp
  , login
  , currentUser
  , updateProfile
  , logout
  )
where

import Application.Port.UserRepository (UserRepository)
import qualified Application.Port.UserRepository as Repository
import Crypto.BCrypt
  ( hashPasswordUsingPolicy
  , slowerBcryptHashingPolicy
  , validatePassword
  )
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Domain.User
  ( AuthError (..)
  , ProfileUpdate
  , SignUpInput (..)
  , User (..)
  , UserProfile (..)
  , userPasswordHash
  , userProfile
  , validateProfileUpdate
  , validateSignUpInput
  )

data Session = Session
  { sessionToken :: Text
  , sessionUser :: UserProfile
  }
  deriving (Eq, Show)

signUp :: UserRepository IO -> SignUpInput -> IO (Either AuthError Session)
signUp repository input =
  case validateSignUpInput input of
    Left err -> pure (Left err)
    Right validInput -> do
      existing <- Repository.findUserByEmail repository (signUpEmail validInput)
      case existing of
        Just _ -> pure (Left DuplicateEmail)
        Nothing -> do
          passwordHash <- hashSecret (signUpPassword validInput)
          user <-
            Repository.createStoredUser
              repository
              (signUpEmail validInput)
              (signUpDisplayName validInput)
              passwordHash
          createSession repository (userProfile user)

login :: UserRepository IO -> Text -> Text -> IO (Either AuthError Session)
login repository email password = do
  found <- Repository.findUserByEmail repository email
  case found of
    Nothing -> pure (Left InvalidCredentials)
    Just user ->
      if validatePassword (Text.encodeUtf8 (userPasswordHash user)) (Text.encodeUtf8 password)
        then createSession repository (userProfile user)
        else pure (Left InvalidCredentials)

currentUser :: UserRepository IO -> Text -> IO (Either AuthError UserProfile)
currentUser repository token = do
  found <- Repository.findUserBySession repository token
  pure (maybe (Left AuthenticationRequired) (Right . userProfile) found)

updateProfile
  :: UserRepository IO -> Text -> ProfileUpdate -> IO (Either AuthError UserProfile)
updateProfile repository token update =
  case validateProfileUpdate update of
    Left err -> pure (Left err)
    Right validUpdate -> do
      found <- Repository.findUserBySession repository token
      case found of
        Nothing -> pure (Left AuthenticationRequired)
        Just user -> do
          updated <- Repository.updateStoredProfile repository (userId user) validUpdate
          pure (maybe (Left AuthenticationRequired) (Right . userProfile) updated)

logout :: UserRepository IO -> Text -> IO ()
logout = Repository.deleteStoredSession

createSession :: UserRepository IO -> UserProfile -> IO (Either AuthError Session)
createSession repository profile = do
  token <- hashSecret (profileEmail profile <> ":session")
  Repository.createStoredSession repository (profileId profile) token
  pure (Right (Session token profile))

hashSecret :: Text -> IO Text
hashSecret secret = do
  hashed <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (Text.encodeUtf8 secret)
  case hashed of
    Just value -> pure (Text.decodeUtf8 value)
    Nothing -> fail "bcrypt could not generate a hash"
