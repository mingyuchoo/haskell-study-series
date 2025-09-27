{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Presentation.Api.ApiHandler
    ( API
    , ChatInput (..)
    , ChatOutput (..)
    , apiServer
    ) where

import           Application.Interfaces.ChatApplicationService (ChatApplicationService (..))
import           Control.Monad.IO.Class                        (liftIO)
import           Data.Aeson                                    (FromJSON,
                                                                ToJSON)
import           Data.Text                                     (Text)
import qualified Data.Text                                     as Text
import           Data.Time.Clock                               (getCurrentTime)
import           Data.Time.Format                              (defaultTimeLocale,
                                                                formatTime)
import           Data.UUID                                     (UUID)
import           Flow                                          ((<|))
import           GHC.Generics                                  (Generic)
import           Servant

-- | Input type for chat requests
data ChatInput = ChatInput { inputMessage :: Text
                             -- ^ User's message text
                           , sessionId    :: Maybe UUID
                             -- ^ Optional session ID for conversation continuity
                           }
     deriving (Eq, Generic, Show)

instance ToJSON ChatInput
instance FromJSON ChatInput

-- | Output type for chat responses
data ChatOutput = ChatOutput { outputMessage   :: Text
                               -- ^ Response message from the assistant
                             , outputSessionId :: UUID
                               -- ^ Session ID for the conversation
                             }
     deriving (Eq, Generic, Show)

instance ToJSON ChatOutput
instance FromJSON ChatOutput

-- | Health info response type for the health check endpoint
data HealthInfo = HealthInfo { status    :: Text
                               -- ^ Service status (UP/DOWN)
                             , version   :: Text
                               -- ^ API version
                             , uptime    :: Int
                               -- ^ Service uptime in seconds
                             , timestamp :: Text
                               -- ^ Current timestamp
                             }
     deriving (Eq, Generic, Show)

instance ToJSON HealthInfo
instance FromJSON HealthInfo

-- | API definition with chat endpoint, health check, and static file serving
type API = "api" :> "chat" :> ReqBody '[JSON] ChatInput :> Post '[JSON] ChatOutput
      :<|> "health" :> Get '[JSON] HealthInfo
      :<|> Raw

-- | API server implementation that combines all handlers
apiServer :: ChatApplicationService s => s -> Server API
apiServer chatAppService =
    chatHandler chatAppService :<|> healthHandler :<|> serveDirectoryFileServer "static"

-- | Health check endpoint handler
healthHandler :: Handler HealthInfo
healthHandler = do
    -- Get current time in ISO 8601 format
    currentTime <- liftIO <| Text.pack <$> formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" <$> getCurrentTime

    -- Return health information
    return <| HealthInfo
        { status    = "UP"
        , version   = "0.1.0.0"
        , uptime    = 0  -- This would need a proper implementation to track server start time
        , timestamp = currentTime
        }

-- | Chat endpoint handler that processes user messages
chatHandler :: ChatApplicationService s => s -> ChatInput -> Handler ChatOutput
chatHandler chatAppService chatInput = do
    -- Process the chat request through the application service
    (response, sessionUUID) <- liftIO <| handleChatRequest
        chatAppService
        (inputMessage chatInput)
        (sessionId chatInput)

    -- Return the formatted response
    return <| ChatOutput
        { outputMessage = response
        , outputSessionId = sessionUUID
        }
