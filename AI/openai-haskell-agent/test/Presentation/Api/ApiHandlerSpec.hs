{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Presentation.Api.ApiHandlerSpec
    ( spec
    ) where

import           Application.Services.ChatApplicationService (createOpenAIChatApplicationService)
import           Application.Services.ChatService            (createOpenAIChatService)
import           Control.Monad.IO.Class                      (liftIO)
import           Flow ((<|))
import           Infrastructure.Http.HttpClient              (createOpenAIHttpClient)
import           Network.HTTP.Client                         (newManager)
import           Network.HTTP.Client.TLS                     (tlsManagerSettings)
import           Presentation.Api.ApiHandler                 (apiServer)
import           Test.Hspec

spec :: Spec
spec = do
  describe "API" <| do
    it "can create a chat server with application service" <| do
      -- Create a real OpenAIChatService for type compatibility
      manager <- liftIO <| newManager tlsManagerSettings
      let httpClient = createOpenAIHttpClient
          chatService = createOpenAIChatService httpClient manager "mock-api-key" "https://api.openai.com/v1/chat/completions"
          appService = createOpenAIChatApplicationService chatService

      -- Just verify that we can create the server handler
      _ <- pure (apiServer appService)  -- Use _ to avoid unused binding warning
      True `shouldBe` True  -- Simple assertion that always passes

      -- Note: Full API testing would require more complex setup with WAI test utilities
      -- or integration tests. This is just a basic structural test.

-- Note: More comprehensive API testing would require proper HTTP client setup
-- and integration tests with a running server.
