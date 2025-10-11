{-# LANGUAGE DeriveGeneric #-}

module Domain.Entities.Message
    ( Message (..)
    , Role (..)
    ) where

import           Data.Text    (Text)

import           GHC.Generics (Generic)

-- | Role type for chat participants
data Role = System -- ^ System role for setting context and instructions
          | User -- ^ User role for queries and inputs
          | Assistant -- ^ Assistant role for responses
     deriving (Eq, Generic, Show)

-- | Message type for chat conversations
data Message = Message { messageRole    :: Role
                         -- ^ Role of the message sender
                       , messageContent :: Text
                         -- ^ Content of the message
                       }
     deriving (Eq, Generic, Show)
