{-# LANGUAGE OverloadedStrings #-}

module Domain.Entities.ChatSpec
    ( spec
    ) where

import           Domain.Entities.Chat
import           Domain.Entities.Message

import           Flow                    ((<|))

import           Test.Hspec

spec :: Spec
spec = do
  describe "ChatRequest" <| do
    it "creates a chat request correctly" <| do
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
      chatMessages request `shouldBe` messages
      chatStream request `shouldBe` True
      chatMaxTokens request `shouldBe` 1000
      chatTemperature request `shouldBe` 0.7
      chatTopP request `shouldBe` 1.0
      chatModel request `shouldBe` "gpt-4o"

  describe "Delta" <| do
    it "creates a delta correctly" <| do
      let delta = Delta (Just "assistant") (Just "Hello")
      deltaRole delta `shouldBe` Just "assistant"
      deltaContent delta `shouldBe` Just "Hello"

    it "handles empty delta correctly" <| do
      let delta = Delta Nothing Nothing
      deltaRole delta `shouldBe` Nothing
      deltaContent delta `shouldBe` Nothing

  describe "Choice" <| do
    it "creates a choice correctly" <| do
      let delta = Delta (Just "assistant") (Just "Hello")
          choice = Choice 0 delta (Just "stop")
      choiceIndex choice `shouldBe` 0
      choiceDelta choice `shouldBe` delta
      choiceFinishReason choice `shouldBe` Just "stop"

  describe "ChatResponse" <| do
    it "creates a chat response correctly" <| do
      let delta = Delta (Just "assistant") (Just "Hello")
          choice = Choice 0 delta (Just "stop")
          response = ChatResponse "resp-123" "chat.completion.chunk" 1617824678 "gpt-4o" [choice]
      chatId response `shouldBe` "resp-123"
      chatObject response `shouldBe` "chat.completion.chunk"
      chatCreated response `shouldBe` 1617824678
      chatResponseModel response `shouldBe` "gpt-4o"
      chatChoices response `shouldBe` [choice]
