module Domain.Interfaces.ChatService
    ( ChatService (..)
    ) where

import           Control.Concurrent.STM.TChan (TChan)

import           Data.Text                    (Text)

import           Domain.Entities.Chat         (ChatRequest)
import           Domain.Entities.Message      (Message)

-- | Interface for chat services that can handle conversations
class ChatService s where
    -- | Create a chat request to the service and return a channel for streaming responses
    createChatRequest :: s -> ChatRequest -> IO (TChan (Maybe Text))

    -- | Process a streaming response from the channel and return the complete text
    processStreamingResponse :: s -> TChan (Maybe Text) -> IO Text

    -- | Run a complete chat conversation and return the updated message history
    runChat :: s -> [Message] -> IO [Message]
