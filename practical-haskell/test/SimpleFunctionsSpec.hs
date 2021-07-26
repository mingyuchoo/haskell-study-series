{-# LANGUAGE UnicodeSyntax #-}

--------------------------------------------------------------------------------
module SimpleFunctionsSpec
    ( spec
    ) where

--------------------------------------------------------------------------------
import           Chapter2.SimpleFunctions
import           Test.Hspec

--------------------------------------------------------------------------------
spec ∷ Spec
spec = do
  describe "firstOrEmpty" $ do
    it "firstOrEmpty [] should be []" $ do
      firstOrEmpty [] `shouldBe` "empty"

    it "firstOrEmpty [\"hello\",\"hola\"] should be \"hello\"" $ do
      firstOrEmpty ["hello","hola"] `shouldBe` "hello"


