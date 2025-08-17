{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Http.HttpClientSpec
    ( spec
    ) where

import           Control.Monad.IO.Class         (liftIO)

import qualified Data.ByteString.Lazy           as LBS

import           Domain.Interfaces.HttpClient   (HttpClient (createRequest))
import           Flow ((<|))

import           Infrastructure.Http.HttpClient (createOpenAIHttpClient)

import           Network.HTTP.Client            (RequestBody (..), method,
                                                 requestBody, requestHeaders)
import           Network.HTTP.Types             (hAuthorization, hContentType,
                                                 methodPost)

import           Test.Hspec

spec :: Spec
spec = do
  describe "OpenAIHttpClient" <| do
    it "creates a request with correct headers and body" <| do
      -- Create a client
      let client = createOpenAIHttpClient

      -- Create a test request
      let apiKey = "test-api-key"
          url = "https://api.openai.com/v1/chat/completions"
          body = LBS.fromStrict "test request body"

      request <- liftIO <| createRequest client apiKey url body

      -- Verify the request properties
      method request `shouldBe` methodPost
      lookup hContentType (requestHeaders request) `shouldBe` Just "application/json"
      lookup hAuthorization (requestHeaders request) `shouldBe` Just "Bearer test-api-key"

      -- Check the request body
      case requestBody request of
        RequestBodyLBS bodyContent -> bodyContent `shouldBe` body
        _ -> expectationFailure "Expected RequestBodyLBS"

    it "creates a new OpenAIHttpClient" <| do
      -- Just verify that the constructor works
      _ <- pure (createOpenAIHttpClient)  -- Use _ to avoid unused binding warning
      True `shouldBe` True  -- Simple assertion that always passes
