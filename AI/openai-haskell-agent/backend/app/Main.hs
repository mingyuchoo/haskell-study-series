module Main
    ( main
    ) where

import           Application.Services.ChatApplicationService (createOpenAIChatApplicationService)
import           Application.Services.ChatService            (createOpenAIChatService)
import           Configuration.Dotenv                        (defaultConfig,
                                                              loadFile)
import           Infrastructure.Http.HttpClient              (createOpenAIHttpClient)
import           Network.HTTP.Client                         (newManager)
import           Network.HTTP.Client.TLS                     (tlsManagerSettings)
import           Presentation.Server.Server                  (runServer)
import           System.Environment                          (getEnv, lookupEnv)
import           Text.Read                                   (readMaybe)



-- | Main function that initializes the application and starts the web server
main :: IO ()
main = do
  -- Load environment variables from .env file
  loadFile defaultConfig

  -- Get port from environment variable or use default (8000)
  portStr <- lookupEnv "PORT"
  let port = case portStr >>= readMaybe of
        Just p  -> p
        Nothing -> 8000

  -- Get API configuration from environment variables
  apiKey <- getEnv "OPENAI_API_KEY"
  apiUrl <- getEnv "OPENAI_API_URL"

  -- Initialize components with dependency injection
  manager <- newManager tlsManagerSettings
  let httpClient = createOpenAIHttpClient
      chatService = createOpenAIChatService httpClient manager apiKey apiUrl
      chatAppService = createOpenAIChatApplicationService chatService

  -- Run the server with the configured port and application service
  runServer port chatAppService
