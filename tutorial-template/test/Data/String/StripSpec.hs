module Data.String.StripSpec (spec) where

--------------------------------------------------------------------------------
import           Data.String.Strip
import           Test.Hspec

--------------------------------------------------------------------------------
spec :: Spec
spec = do
  describe "strip" $ do
    it "removes leading and trailing whitespace" $ do
      strip "\t  foo bar\n" `shouldBe` "foo bar"
--------------------------------------------------------------------------------
