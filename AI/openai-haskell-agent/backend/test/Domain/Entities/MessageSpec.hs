{-# LANGUAGE OverloadedStrings #-}

module Domain.Entities.MessageSpec
    ( spec
    ) where

import           Domain.Entities.Message

import           Flow                    ((<|))

import           Test.Hspec

spec :: Spec
spec = do
  describe "Message" <| do
    it "creates a system message correctly" <| do
      let msg = Message System "This is a system message"
      messageRole msg `shouldBe` System
      messageContent msg `shouldBe` "This is a system message"

    it "creates a user message correctly" <| do
      let msg = Message User "This is a user message"
      messageRole msg `shouldBe` User
      messageContent msg `shouldBe` "This is a user message"

    it "creates an assistant message correctly" <| do
      let msg = Message Assistant "This is an assistant message"
      messageRole msg `shouldBe` Assistant
      messageContent msg `shouldBe` "This is an assistant message"

    it "compares messages for equality correctly" <| do
      let msg1 = Message User "Hello"
          msg2 = Message User "Hello"
          msg3 = Message Assistant "Hello"
          msg4 = Message User "Different content"
      msg1 `shouldBe` msg2
      msg1 `shouldNotBe` msg3
      msg1 `shouldNotBe` msg4
