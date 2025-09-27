{-# LANGUAGE DeriveGeneric #-}

module Domain.Entities.Chat
    ( ChatRequest (..)
    , ChatResponse (..)
    , Choice (..)
    , Delta (..)
    ) where

import           Data.Text               (Text)
import           Domain.Entities.Message (Message)
import           GHC.Generics            (Generic)

-- | Chat request type for the OpenAI API
data ChatRequest = ChatRequest { chatMessages    :: [Message]
                                 -- ^ List of messages in the conversation
                               , chatStream      :: Bool
                                 -- ^ Whether to stream the response
                               , chatMaxTokens   :: Int
                                 -- ^ Maximum number of tokens to generate
                               , chatTemperature :: Double
                                 -- ^ Sampling temperature (0.0-2.0)
                               , chatTopP        :: Double
                                 -- ^ Nucleus sampling parameter
                               , chatModel       :: Text
                                 -- ^ Model identifier
                               }
     deriving (Eq, Generic, Show)

-- | Delta type for streaming responses
data Delta = Delta { deltaRole    :: Maybe Text
                     -- ^ Role in the streaming response
                   , deltaContent :: Maybe Text
                     -- ^ Content chunk in the streaming response
                   }
     deriving (Eq, Generic, Show)

-- | Choice type for responses
data Choice = Choice { choiceIndex        :: Int
                       -- ^ Index of this choice
                     , choiceDelta        :: Delta
                       -- ^ Content delta for streaming
                     , choiceFinishReason :: Maybe Text
                       -- ^ Reason why the model stopped generating
                     }
     deriving (Eq, Generic, Show)

-- | Chat response type from the OpenAI API
data ChatResponse = ChatResponse { chatId            :: Text
                                   -- ^ Unique identifier for the response
                                 , chatObject        :: Text
                                   -- ^ Object type
                                 , chatCreated       :: Int
                                   -- ^ Unix timestamp of creation
                                 , chatResponseModel :: Text
                                   -- ^ Model used for the response
                                 , chatChoices       :: [Choice]
                                   -- ^ List of completion choices
                                 }
     deriving (Eq, Generic, Show)
