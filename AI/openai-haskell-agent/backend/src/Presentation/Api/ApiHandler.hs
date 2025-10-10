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
import           Servant.OpenApi                              (toOpenApi)
import           Data.OpenApi                                 (OpenApi, ToSchema, declareNamedSchema, genericDeclareNamedSchema, defaultSchemaOptions)
import           Data.Proxy                                   (Proxy (..))
import           Servant.HTML.Blaze                           (HTML)
import           Text.Blaze.Html5                             (Html)
import qualified Text.Blaze.Html5                             as H
import qualified Text.Blaze.Html5.Attributes                  as A

-- | Input type for chat requests
data ChatInput = ChatInput { inputMessage :: Text
                             -- ^ User's message text
                           , sessionId    :: Maybe UUID
                             -- ^ Optional session ID for conversation continuity
                           }
     deriving (Eq, Generic, Show)

instance ToJSON ChatInput
instance FromJSON ChatInput
instance ToSchema ChatInput where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

-- | Output type for chat responses
data ChatOutput = ChatOutput { outputMessage   :: Text
                               -- ^ Response message from the assistant
                             , outputSessionId :: UUID
                               -- ^ Session ID for the conversation
                             }
     deriving (Eq, Generic, Show)

instance ToJSON ChatOutput
instance FromJSON ChatOutput
instance ToSchema ChatOutput where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

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
instance ToSchema HealthInfo where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

-- | 내부 실제 비즈니스 API 라우트 (OpenAPI 생성 대상)
type APIRoutes =
       "api" :> "chat" :> ReqBody '[JSON] ChatInput :> Post '[JSON] ChatOutput
  :<|>  "health" :> Get '[JSON] HealthInfo

-- | 전체 API: 비즈니스 라우트 + OpenAPI 스펙 + Swagger UI + 정적 파일
type API =
       APIRoutes
  :<|>  "openapi.json" :> Get '[JSON] OpenApi
  :<|>  "swagger" :> Get '[HTML] Html
  :<|>  Raw

-- | API server implementation that combines all handlers
apiServer :: ChatApplicationService s => s -> Server API
apiServer chatAppService =
    routesServer :<|> openApiHandler :<|> swaggerUi :<|> staticFiles
  where
    routesServer = chatHandler chatAppService :<|> healthHandler
    openApiHandler = pure (toOpenApi (Proxy :: Proxy APIRoutes))
    swaggerUi = pure swaggerUiHtml
    staticFiles = serveDirectoryFileServer "static"

-- | Swagger UI HTML (CDN 사용)
swaggerUiHtml :: Html
swaggerUiHtml = H.docTypeHtml $ do
  H.head $ do
    H.meta H.! A.charset "utf-8"
    H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1"
    H.title "Swagger UI"
    H.link H.! A.rel "stylesheet" H.! A.href "https://unpkg.com/swagger-ui-dist@5/swagger-ui.css"
    H.style "html { box-sizing: border-box; overflow: -moz-scrollbars-vertical; overflow-y: scroll; }\n*, *:before, *:after { box-sizing: inherit; }\nbody { margin:0; background: #fafafa; }"
  H.body $ do
    H.div H.! A.id "swagger-ui" $ mempty
    H.script H.! A.src "https://unpkg.com/swagger-ui-dist@5/swagger-ui-bundle.js" $ mempty
    H.script H.! A.src "https://unpkg.com/swagger-ui-dist@5/swagger-ui-standalone-preset.js" $ mempty
    H.script $ H.preEscapedToHtml ("window.onload = () => { window.ui = SwaggerUIBundle({ url: '/openapi.json', dom_id: '#swagger-ui', presets: [SwaggerUIBundle.presets.apis, SwaggerUIStandalonePreset], layout: 'StandaloneLayout' }); };" :: String)

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
