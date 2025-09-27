module Application.Services.ChatService
    ( OpenAIChatService (..)
    , createOpenAIChatService
    ) where

import           Control.Concurrent.STM              (atomically)
import           Control.Concurrent.STM.TChan        (readTChan)
import qualified Data.Aeson
import qualified Data.ByteString.Lazy                as LBS
import           Data.Text                           (pack)
import qualified Data.Text                           as T
import qualified Data.Text.IO                        as TIO
import           Domain.Entities.Chat                (ChatRequest (..))
import           Domain.Entities.Message             (Message (..), Role (..))
import           Domain.Interfaces.ChatService       (ChatService (..))
import           Domain.Interfaces.HttpClient        (HttpClient (..))
import           Flow                                ((<|))
import           Infrastructure.Http.HttpClient      (OpenAIHttpClient)
import           Infrastructure.OpenAI.OpenAIService (toJsonChatRequest)
import           Network.HTTP.Client                 (Manager)
import           System.Environment                  (getEnv)
import           System.IO                           (hFlush, stdout)
import           Text.Read                           (readMaybe)

-- | OpenAI Chat Service implementation
data OpenAIChatService = OpenAIChatService { ocsHttpClient :: OpenAIHttpClient
                                             -- ^ HTTP client for API requests
                                           , ocsManager    :: Manager
                                             -- ^ HTTP connection manager
                                           , ocsApiKey     :: String
                                             -- ^ OpenAI API key
                                           , ocsApiUrl     :: String
                                             -- ^ OpenAI API URL
                                           }

-- | Create a new OpenAI Chat Service
createOpenAIChatService :: OpenAIHttpClient -> Manager -> String -> String -> OpenAIChatService
createOpenAIChatService = OpenAIChatService

instance ChatService OpenAIChatService where
    createChatRequest service chatRequest = do
        let httpClient = ocsHttpClient service
            manager = ocsManager service
            apiKey = ocsApiKey service
            apiUrl = ocsApiUrl service
            requestBody = encodeRequest chatRequest

        -- Create the request and stream the response
        request <- createRequest httpClient apiKey apiUrl requestBody
        streamResponse httpClient manager request

    processStreamingResponse _ responseChan = loop T.empty
      where
        loop acc = do
            chunk <- atomically <| readTChan responseChan
            case chunk of
                Just content -> do
                    TIO.putStr content
                    hFlush stdout
                    loop (acc <> content)
                Nothing -> do
                    TIO.putStrLn (pack "")  -- End of response
                    return acc

    runChat service messages = do
        -- Get model configuration from environment variables
        modelName <- pack <$> getEnv "OPENAI_API_MODEL"
        streamStr <- getEnv "OPENAI_API_STREAM"
        maxTokensStr <- getEnv "OPENAI_API_MAX_TOKENS"
        temperatureStr <- getEnv "OPENAI_API_TEMPERATURE"
        topPStr <- getEnv "OPENAI_API_TOP_P"

        -- Parse numeric values with defaults if parsing fails
        let readWithDefault def str = maybe def id (readMaybe str)
            stream = readWithDefault True streamStr
            maxTokens = readWithDefault 4096 maxTokensStr
            temperature = readWithDefault 1.0 temperatureStr
            topP = readWithDefault 1.0 topPStr

            -- Create chat request with configuration
            chatRequest = ChatRequest
                { chatMessages = messages
                , chatStream = stream
                , chatMaxTokens = maxTokens
                , chatTemperature = temperature
                , chatTopP = topP
                , chatModel = modelName
                }

        -- Process request and get response
        responseChan <- createChatRequest service chatRequest
        assistantResponse <- processStreamingResponse service responseChan
        let assistantMessage = Message Assistant assistantResponse

        -- Return updated message history
        return <| messages <> [assistantMessage]

-- | Helper function to encode a ChatRequest to JSON for the API
encodeRequest :: ChatRequest -> LBS.ByteString
encodeRequest = Data.Aeson.encode . toJsonChatRequest
