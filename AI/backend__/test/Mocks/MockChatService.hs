{-# LANGUAGE OverloadedStrings #-}

module Mocks.MockChatService
    ( MockChatService (..)
    , createMockChatService
    ) where

import           Control.Concurrent.STM        (atomically)
import           Control.Concurrent.STM.TChan  (newTChanIO, readTChan,
                                                writeTChan)

import           Data.Text                     (Text)

import           Domain.Entities.Message       (Message (..), Role (..))
import           Domain.Interfaces.ChatService (ChatService (..))

import           Flow                          ((<|))

-- | Mock implementation of ChatService for testing
data MockChatService = MockChatService { mockResponses :: [Text]
                                         -- ^ Predefined responses for testing
                                       }

-- | Create a new MockChatService with predefined responses
createMockChatService :: [Text] -> MockChatService
createMockChatService = MockChatService

instance ChatService MockChatService where
    createChatRequest service _ = do
        responseChan <- newTChanIO

        -- Write the first mock response to the channel
        case mockResponses service of
            (response:_) -> atomically <| writeTChan responseChan (Just response)
            [] -> atomically <| writeTChan responseChan (Just "Mock response")

        -- Signal end of stream
        atomically <| writeTChan responseChan Nothing

        return responseChan

    processStreamingResponse _ responseChan = do
        -- Read the first response from the channel and return it
        response <- atomically <| readTChan responseChan
        case response of
            Just text -> return text
            Nothing   -> return ""

    runChat service messages = do
        -- Get the first mock response or use a default
        let response = case mockResponses service of
                (r:_) -> r
                []    -> "Mock assistant response"

        -- Create and return a new message history with the assistant response
        return <| messages <> [Message Assistant response]
