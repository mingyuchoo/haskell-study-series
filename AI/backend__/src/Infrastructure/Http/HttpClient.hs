{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Http.HttpClient
    ( OpenAIHttpClient (..)
    , createOpenAIHttpClient
    ) where

import           Control.Concurrent.Async            (async)
import           Control.Concurrent.STM              (atomically)
import           Control.Concurrent.STM.TChan        (TChan, newTChanIO,
                                                      writeTChan)
import           Control.Monad                       (forM_, unless, void)

import           Data.ByteString                     (ByteString)
import qualified Data.ByteString.Char8               as BS8
import           Data.Text                           (Text)

import           Domain.Entities.Chat                (ChatResponse (..),
                                                      Choice (..), Delta (..),
                                                      chatChoices, choiceDelta,
                                                      deltaContent)
import           Domain.Interfaces.HttpClient        (HttpClient (..))

import           Flow                                ((<|))

import           Infrastructure.OpenAI.OpenAIService (parseChatStreamLine)

import           Network.HTTP.Client                 (BodyReader,
                                                      RequestBody (..),
                                                      Response, brRead, httpLbs,
                                                      method, parseRequest,
                                                      requestBody,
                                                      requestHeaders,
                                                      responseBody,
                                                      withResponse)
import           Network.HTTP.Types                  (hAuthorization,
                                                      hContentType, methodPost)

-- | OpenAI HTTP Client implementation for making API requests
data OpenAIHttpClient = OpenAIHttpClient

-- | Create a new OpenAI HTTP Client
createOpenAIHttpClient :: OpenAIHttpClient
createOpenAIHttpClient = OpenAIHttpClient

instance HttpClient OpenAIHttpClient where
    createRequest _ apiKey url body = do
        -- Create the base request and add headers and body
        initialRequest <- parseRequest url
        return <| initialRequest
            { method = methodPost
            , requestHeaders =
                [ (hContentType, "application/json")
                , (hAuthorization, BS8.pack <| "Bearer " <> apiKey)
                ]
            , requestBody = RequestBodyLBS body
            }

    executeRequest _ manager request = do
        -- Execute the request and return the response body
        responseBody <$> httpLbs request manager

    streamResponse _ manager request = do
        responseChan <- newTChanIO

        -- Start a separate thread to handle the response
        void <| async <| withResponse request manager <| \response -> do
            -- Process the response body
            processResponseBody response responseChan
            -- Signal the end of the response
            atomically <| writeTChan responseChan Nothing

        return responseChan

-- | Process the streaming response body
processResponseBody :: Response BodyReader -> TChan (Maybe Text) -> IO ()
processResponseBody response chan = processStream (responseBody response) chan

-- | Process a stream of data recursively
processStream :: BodyReader -> TChan (Maybe Text) -> IO ()
processStream reader chan = do
    chunk <- brRead reader
    unless (BS8.null chunk) <| do
        processChunk chunk chan  -- Process this chunk
        processStream reader chan  -- Process next chunk recursively

-- | Process a chunk of data by splitting into lines
processChunk :: ByteString -> TChan (Maybe Text) -> IO ()
processChunk chunk chan = mapM_ (processLine chan) (BS8.split '\n' chunk)

-- | Process a single line from the response
processLine :: TChan (Maybe Text) -> ByteString -> IO ()
processLine chan line =
    -- Skip empty lines and process non-empty ones
    unless (BS8.null line) $
        -- Check for [DONE] message or parse the line
        if BS8.pack "data: [DONE]" `BS8.isPrefixOf` line
            then return ()  -- End of stream marker
            else case parseChatStreamLine line of
                Just chatResponse ->
                    -- Process each choice in the response
                    forM_ (chatChoices chatResponse) <| \choice ->
                        -- Extract and send content if available
                        case deltaContent (choiceDelta choice) of
                            Just content -> atomically <| writeTChan chan (Just content)
                            Nothing      -> return ()
                Nothing -> return ()
