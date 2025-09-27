{-# LANGUAGE OverloadedStrings #-}

module Mocks.MockHttpClient
    ( MockHttpClient (..)
    , createMockHttpClient
    ) where

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (newTChanIO, writeTChan)
import qualified Data.ByteString.Lazy         as LBS
import           Data.Text                    (Text)
import           Domain.Interfaces.HttpClient (HttpClient (..))
import           Flow ((<|))
import           Network.HTTP.Client          (RequestBody (..), method,
                                               parseRequest, requestBody,
                                               requestHeaders)
import           Network.HTTP.Types           (methodPost)

-- | Mock implementation of HttpClient for testing
data MockHttpClient = MockHttpClient { mockResponseBody    :: LBS.ByteString
                                       -- ^ Predefined response body for executeRequest
                                     , mockStreamResponses :: [Text]
                                       -- ^ Predefined streaming responses
                                     }

-- | Create a new MockHttpClient with predefined responses
createMockHttpClient :: LBS.ByteString -> [Text] -> MockHttpClient
createMockHttpClient = MockHttpClient

instance HttpClient MockHttpClient where
    createRequest _ _ url body = do
        -- Create a simple request with the provided URL and body
        initialRequest <- parseRequest url
        return <| initialRequest
            { method = methodPost
            , requestHeaders = [("Authorization", "Bearer " <> "mock-api-key")]
            , requestBody = RequestBodyLBS body
            }

    executeRequest client _ _ = do
        -- Return the predefined response body
        return <| mockResponseBody client

    streamResponse client _ _ = do
        -- Create a channel and write the predefined streaming responses
        responseChan <- newTChanIO

        -- Write each mock response to the channel
        mapM_ (\response -> atomically <| writeTChan responseChan (Just response))
              (mockStreamResponses client)

        -- Signal end of stream
        atomically <| writeTChan responseChan Nothing

        return responseChan
