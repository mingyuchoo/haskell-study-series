{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.OpenAI.OpenAIServiceSpec
    ( spec
    ) where

import qualified Data.ByteString.Char8               as BS8

import           Domain.Entities.Chat                (ChatRequest (..),
                                                      ChatResponse (..),
                                                      Choice (..), Delta (..))
import           Domain.Entities.Message             (Message (..), Role (..))

import           Flow                                ((<|))

import           Infrastructure.OpenAI.OpenAIService

import           Test.Hspec

spec :: Spec
spec = do
  describe "OpenAIService JSON conversions" <| do
    it "converts domain Message to JsonMessage correctly" <| do
      let message = Message System "You are a helpful assistant"
          jsonMessage = toJsonMessage message

      jsonMessageRole jsonMessage `shouldBe` JsonSystem
      jsonMessageContent jsonMessage `shouldBe` "You are a helpful assistant"

    it "converts domain ChatRequest to JsonChatRequest correctly" <| do
      let messages = [Message System "You are a helpful assistant",
                     Message User "Hello"]
          request = ChatRequest {
            chatMessages = messages,
            chatStream = True,
            chatMaxTokens = 1000,
            chatTemperature = 0.7,
            chatTopP = 1.0,
            chatModel = "gpt-4o"
          }
          jsonRequest = toJsonChatRequest request

      length (jsonChatMessages jsonRequest) `shouldBe` 2
      jsonChatStream jsonRequest `shouldBe` True
      jsonChatMaxTokens jsonRequest `shouldBe` 1000
      jsonChatTemperature jsonRequest `shouldBe` 0.7
      jsonChatTopP jsonRequest `shouldBe` 1.0
      jsonChatModel jsonRequest `shouldBe` "gpt-4o"

    it "parses a streaming response line correctly" <| do
      let responseJson = "{\"id\":\"chatcmpl-123\",\"object\":\"chat.completion.chunk\",\"created\":1677652288,\"model\":\"gpt-4o\",\"choices\":[{\"index\":0,\"delta\":{\"content\":\"Hello\"},\"finish_reason\":null}]}"
          responseLine = BS8.pack <| "data: " <> responseJson

      case parseChatStreamLine responseLine of
        Just response -> do
          chatId response `shouldBe` "chatcmpl-123"
          chatObject response `shouldBe` "chat.completion.chunk"
          chatCreated response `shouldBe` 1677652288
          chatResponseModel response `shouldBe` "gpt-4o"
          length (chatChoices response) `shouldBe` 1

          -- Use pattern matching with a case expression to handle all patterns
          case chatChoices response of
            [] -> expectationFailure "Expected non-empty choices list"
            (choice:_) -> do
              choiceIndex choice `shouldBe` 0
              deltaContent (choiceDelta choice) `shouldBe` Just "Hello"
              choiceFinishReason choice `shouldBe` Nothing

        Nothing -> expectationFailure "Failed to parse response line"

    it "handles [DONE] marker correctly" <| do
      let doneLine = BS8.pack "data: [DONE]"
      parseChatStreamLine doneLine `shouldBe` Nothing

    it "handles empty lines correctly" <| do
      let emptyLine = BS8.pack ""
      parseChatStreamLine emptyLine `shouldBe` Nothing
