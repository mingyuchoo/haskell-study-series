{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.OpenAI.OpenAIService
    ( JsonChatRequest (..)
    , JsonChatResponse (..)
    , JsonChoice (..)
    , JsonDelta (..)
    , JsonMessage (..)
    , JsonRole (..)
    , OpenAIService
    , createOpenAIService
    , parseChatStreamLine
    , toJsonChatRequest
    , toJsonMessage
    ) where



import           Data.Aeson              (FromJSON (..), ToJSON (..),
                                          Value (..), decode, object,
                                          withObject, (.:), (.:?), (.=))
import qualified Data.Aeson.Key          as Key
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as BS8
import qualified Data.ByteString.Lazy    as LBS
import           Data.Text               (Text, pack)
import qualified Data.Text               as T

import           Domain.Entities.Chat    (ChatRequest (..), ChatResponse (..),
                                          Choice (..), Delta (..))
import           Domain.Entities.Message (Message (..), Role (..))

import           Flow                    ((<|))

import           GHC.Generics            (Generic)

import           Network.HTTP.Client     (Manager)

-- | OpenAI Service for handling OpenAI API requests
data OpenAIService = OpenAIService { _oasManager :: Manager
                                     -- ^ HTTP connection manager
                                   , _oasApiKey  :: String
                                     -- ^ OpenAI API key
                                   , _oasApiUrl  :: String
                                     -- ^ OpenAI API URL
                                   }

-- | Create a new OpenAI Service
createOpenAIService :: Manager -> String -> String -> OpenAIService
createOpenAIService = OpenAIService

-- | JSON representation of Role for OpenAI API
data JsonRole = JsonSystem -- ^ System role
              | JsonUser -- ^ User role
              | JsonAssistant -- ^ Assistant role
     deriving (Eq, Generic, Show)

-- | Convert JsonRole to JSON string
instance ToJSON JsonRole where
    toJSON JsonSystem    = String (pack "system")
    toJSON JsonUser      = String (pack "user")
    toJSON JsonAssistant = String (pack "assistant")

-- | Parse JSON string to JsonRole
instance FromJSON JsonRole where
    parseJSON = withObject "JsonRole" <| \v -> do
        role <- v .: Key.fromString "role"
        case (role :: Text) of
            t | t == pack "system"    -> return JsonSystem
            t | t == pack "user"      -> return JsonUser
            t | t == pack "assistant" -> return JsonAssistant
            _                         -> fail <| "Unknown role: " <> T.unpack role

-- | Convert between domain Role and JsonRole
toJsonRole :: Role -> JsonRole
toJsonRole System    = JsonSystem
toJsonRole User      = JsonUser
toJsonRole Assistant = JsonAssistant

-- | Convert from JsonRole to domain Role
_fromJsonRole :: JsonRole -> Role
_fromJsonRole JsonSystem    = System
_fromJsonRole JsonUser      = User
_fromJsonRole JsonAssistant = Assistant

-- | JSON representation of Message for OpenAI API
data JsonMessage = JsonMessage { jsonMessageRole    :: JsonRole
                                 -- ^ Role of the message sender
                               , jsonMessageContent :: Text
                                 -- ^ Content of the message
                               }
     deriving (Eq, Generic, Show)

-- | Convert JsonMessage to JSON
instance ToJSON JsonMessage where
    toJSON (JsonMessage role content) =
        object [ Key.fromString "role" .= role
               , Key.fromString "content" .= content
               ]

-- | Parse JSON to JsonMessage
instance FromJSON JsonMessage where
    parseJSON = withObject "JsonMessage" <| \v -> JsonMessage
        <$> v .: Key.fromString "role"
        <*> v .: Key.fromString "content"

-- | Convert from domain Message to JsonMessage
toJsonMessage :: Message -> JsonMessage
toJsonMessage (Message role content) =
    JsonMessage (toJsonRole role) content

-- | Convert from JsonMessage to domain Message
_fromJsonMessage :: JsonMessage -> Message
_fromJsonMessage (JsonMessage role content) =
    Message (_fromJsonRole role) content

-- | JSON representation of ChatRequest for OpenAI API
data JsonChatRequest = JsonChatRequest { jsonChatMessages    :: [JsonMessage]
                                         -- ^ List of messages in the conversation
                                       , jsonChatStream      :: Bool
                                         -- ^ Whether to stream the response
                                       , jsonChatMaxTokens   :: Int
                                         -- ^ Maximum number of tokens to generate
                                       , jsonChatTemperature :: Double
                                         -- ^ Sampling temperature (0.0-2.0)
                                       , jsonChatTopP        :: Double
                                         -- ^ Nucleus sampling parameter
                                       , jsonChatModel       :: Text
                                         -- ^ Model identifier
                                       }
     deriving (Eq, Generic, Show)

-- | Convert JsonChatRequest to JSON
instance ToJSON JsonChatRequest where
    toJSON (JsonChatRequest msgs stream maxTokens temp topP model) =
        object [ Key.fromString "messages" .= msgs
               , Key.fromString "stream" .= stream
               , Key.fromString "max_tokens" .= maxTokens
               , Key.fromString "temperature" .= temp
               , Key.fromString "top_p" .= topP
               , Key.fromString "model" .= model
               ]

-- | Convert from domain ChatRequest to JsonChatRequest for API
toJsonChatRequest :: ChatRequest -> JsonChatRequest
toJsonChatRequest (ChatRequest msgs stream maxTokens temp topP model) =
    JsonChatRequest (map toJsonMessage msgs) stream maxTokens temp topP model

-- | JSON representation of Delta for OpenAI API streaming responses
data JsonDelta = JsonDelta { jsonDeltaRole    :: Maybe JsonRole
                             -- ^ Role in the streaming response
                           , jsonDeltaContent :: Maybe Text
                             -- ^ Content chunk in the streaming response
                           }
     deriving (Eq, Generic, Show)

-- | Parse JSON to JsonDelta
instance FromJSON JsonDelta where
    parseJSON = withObject "JsonDelta" <| \v -> JsonDelta
        <$> v .:? Key.fromString "role"
        <*> v .:? Key.fromString "content"

-- | Convert from JsonDelta to domain Delta
fromJsonDelta :: JsonDelta -> Delta
fromJsonDelta (JsonDelta mRole mContent) =
    Delta (fmap (pack . show) mRole) mContent

-- | JSON representation of Choice for OpenAI API responses
data JsonChoice = JsonChoice { jsonChoiceIndex        :: Int
                               -- ^ Index of this choice
                             , jsonChoiceDelta        :: JsonDelta
                               -- ^ Content delta for streaming
                             , jsonChoiceFinishReason :: Maybe Text
                               -- ^ Reason why the model stopped generating
                             }
     deriving (Eq, Generic, Show)

-- | Parse JSON to JsonChoice
instance FromJSON JsonChoice where
    parseJSON = withObject "JsonChoice" <| \v -> JsonChoice
        <$> v .: Key.fromString "index"
        <*> v .: Key.fromString "delta"
        <*> v .:? Key.fromString "finish_reason"

-- | Convert from JsonChoice to domain Choice
fromJsonChoice :: JsonChoice -> Choice
fromJsonChoice (JsonChoice index delta finishReason) =
    Choice index (fromJsonDelta delta) finishReason

-- | JSON representation of ChatResponse from the OpenAI API
data JsonChatResponse = JsonChatResponse { jsonChatId            :: Text
                                           -- ^ Unique identifier for the response
                                         , jsonChatObject        :: Text
                                           -- ^ Object type
                                         , jsonChatCreated       :: Int
                                           -- ^ Unix timestamp of creation
                                         , jsonChatResponseModel :: Text
                                           -- ^ Model used for the response
                                         , jsonChatChoices       :: [JsonChoice]
                                           -- ^ List of completion choices
                                         }
     deriving (Eq, Generic, Show)

-- | Parse JSON to JsonChatResponse
instance FromJSON JsonChatResponse where
    parseJSON = withObject "JsonChatResponse" <| \v -> JsonChatResponse
        <$> v .: Key.fromString "id"
        <*> v .: Key.fromString "object"
        <*> v .: Key.fromString "created"
        <*> v .: Key.fromString "model"
        <*> v .: Key.fromString "choices"

-- | Convert from JsonChatResponse to domain ChatResponse
fromJsonChatResponse :: JsonChatResponse -> ChatResponse
fromJsonChatResponse (JsonChatResponse id' object' created model choices) =
    ChatResponse id' object' created model (map fromJsonChoice choices)

-- | Parse a streaming response line from the API
parseChatStreamLine :: ByteString -> Maybe ChatResponse
parseChatStreamLine line = do
    -- Skip empty lines and "data: [DONE]" messages
    let trimmed = BS8.dropWhile (== ' ') line
    if BS8.null trimmed || trimmed == BS8.pack "data: [DONE]"
        then Nothing
        else do
            -- Remove "data: " prefix if present and parse JSON
            let content = if BS8.pack "data: " `BS8.isPrefixOf` trimmed
                          then BS8.drop 6 trimmed
                          else trimmed
                decoded = decode (LBS.fromStrict content) :: Maybe JsonChatResponse

            -- Convert to domain type if successfully parsed
            fmap fromJsonChatResponse decoded
