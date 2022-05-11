import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Given" $ do
    describe "When" $ do
      describe "Then" $ do
        it "it should be True" $ do
          True `shouldBe` True
