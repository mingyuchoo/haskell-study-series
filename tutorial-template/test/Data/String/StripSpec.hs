module Data.String.StripSpec (spec) where


import Data.Char
import Test.Hspec

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

spec :: Spec
spec = do
  describe "strip" $ do
    it "removes leading and trailing whitespace" $ do
      strip "\t  foo bar\n" `shouldBe` "foo bar"
