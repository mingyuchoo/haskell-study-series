{-# LANGUAGE DeriveGeneric #-}

module Domain.Entities
    ( ChatMessage (..)
    , ChatRole (..)
    , ChatSession (..)
    , SessionId
    ) where

import           Data.Text    (Text)
import           Data.UUID    (UUID)
import           GHC.Generics

type SessionId = UUID

data ChatRole = SystemRole | UserRole | AssistantRole
    deriving (Eq, Show, Generic)

data ChatMessage = ChatMessage
    { messageRole    :: ChatRole
    , messageContent :: Text
    } deriving (Show, Generic)

data ChatSession = ChatSession
    { sessionId       :: SessionId
    , sessionMessages :: [ChatMessage]
    } deriving (Show, Generic)
