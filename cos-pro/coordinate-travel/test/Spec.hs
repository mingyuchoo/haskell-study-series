import Test.Hspec
import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Given matrix size 5, inital coordinate-travel (1,1)" $ do
    describe "When I command to move R" $ do
      it "Then it Should be (1,1)" $ do
        move 5 (1,1) "L" `shouldBe` (1,1)
    describe "When I command to move R" $ do
      it "Then it Should be (1,2)" $ do
        move 5 (1,1) "R" `shouldBe` (1,2)
    describe "When I command to move R" $ do
      it "Then it Should be (1,1)" $ do
        move 5 (1,1) "U" `shouldBe` (1,1)
    describe "When I command to move R" $ do
      it "Then it Should be (2,1)" $ do
        move 5 (1,1) "D" `shouldBe` (2,1)
