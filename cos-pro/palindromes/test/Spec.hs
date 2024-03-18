-- {-# OPTIONS_GHC -F -pgmF doctest-discover #-}
-- {-# OPTIONS_GHC -F -pgmF hspec-discover   #-}

import Test.Hspec
import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Given Prelude" $ do
      context "when use `read` function" $ do
        it "should parse integers" $ do
          read "10" `shouldBe` (10 :: Int)
        it "should parse floating-point numbers" $ do
          read "2.5" `shouldBe` (2.5 :: Float)
    describe "Given Lib" $ do
      context "when use `isPalindrome` function" $ do
        it "should be succeeded" $ do
          isPalindrome "bob" `shouldBe` True
          isPalindrome "hello" `shouldBe` False
      context "when use `ignoreCase` function" $ do
        it "should be succeeded" $ do
          ignoreCase "abcdefg" `shouldBe` "abcdefg"
          ignoreCase "aBcDeFg" `shouldBe` "abcdefg"
          ignoreCase "ABCDEFG" `shouldBe` "abcdefg"
      context "when use `makeWord` function" $ do
        it "should be succededed" $ do
          makeWord "madam, I'm Adam!" `shouldBe` "madamImAdam"
