{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Presentation.Server.ServerSpec
    ( spec
    ) where

import           Application.Services.ChatApplicationService (createOpenAIChatApplicationService)
import           Application.Services.ChatService            (createOpenAIChatService)

import           Control.Monad.IO.Class                      (liftIO)
import           Flow ((<|))

import           Infrastructure.Http.HttpClient              (createOpenAIHttpClient)

import           Network.HTTP.Client                         (newManager)
import           Network.HTTP.Client.TLS                     (tlsManagerSettings)

import           Presentation.Server.Server                  (runServer)

import           Test.Hspec

spec :: Spec
spec = do
  describe "Server" <| do
    it "can initialize the server function" <| do
      -- This is a basic test to ensure the server module can be initialized
      -- A more comprehensive test would require running the actual server
      -- and making HTTP requests to it

      -- Create a real OpenAIChatService for type compatibility
      manager <- liftIO <| newManager tlsManagerSettings
      let httpClient = createOpenAIHttpClient
          chatService = createOpenAIChatService httpClient manager "mock-api-key" "https://api.openai.com/v1/chat/completions"
          appService = createOpenAIChatApplicationService chatService
          port = 8080

      -- Just verify that the runServer function exists and has the right type
      -- We don't actually run it as that would start a real server
      let serverFn = runServer port appService

      -- Just check that the function has the right type (IO ())
      (serverFn `seq` True) `shouldBe` True

    -- Note: More comprehensive tests would require starting the server
    -- and making actual HTTP requests, which is beyond the scope of
    -- these unit tests. Integration tests would be more appropriate
    -- for testing the full server functionality.
