import Lib
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Given matrix1" $ do
    describe "When apply `solution1` function to maxtrix1" $ do
      describe "Then it" $ do
        it "Should be evaluated as answer1" $ do
          (solution1 matrix1) `shouldBe` answer1

  describe "Given matrixw" $ do
    describe "When apply `solution1` function to maxtrix2" $ do
      describe "Then it" $ do
        it "Should be evaluated as answer2" $ do
          (solution1 matrix2) `shouldBe` answer2

  describe "Given matrix1" $ do
    describe "When apply `solution2` function to maxtrix1" $ do
      describe "Then it" $ do
        it "Should be evaluated as answer1" $ do
          (solution2 matrix1) `shouldBe` answer1

  describe "Given matrixw" $ do
    describe "When apply `solution2` function to maxtrix2" $ do
      describe "Then it" $ do
        it "Should be evaluated as answer2" $ do
          (solution2 matrix2) `shouldBe` answer2
