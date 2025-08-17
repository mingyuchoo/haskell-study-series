import           Data(grid, languages)
import           Lib ( cell2char
                     , findWord
                     , findWords
                     , formatGrid
                     , gridWithCoords
                     , Cell
                     , Grid
                     )
import           Test.Hspec (hspec, describe, it, shouldBe)

-- |
--
--
gwc :: Grid Cell
gwc = gridWithCoords grid

-- |
--
--
testFindWord :: String -> IO ()
testFindWord word =
  case findWord gwc word of
    Nothing       -> return ()
    (Just result) -> map cell2char result `shouldBe` word

-- |
--
--
main :: IO ()
main = hspec $ do
  describe "formatGrid" $ do
    it "Should concatenate every line with a newline" $ do
      formatGrid (gridWithCoords ["abc", "def", "ghi"]) `shouldBe` "abc\ndef\nghi\n"

  describe "findWord" $ do
    it "Should find words that exist on the Grid" $ do
      testFindWord "HAKELL"
      testFindWord "PERL"
    it "Should not find words that do not exist on the Grid" $ do
      findWord gwc "HAMSTER" `shouldBe` Nothing

  describe "findWords" $ do
    it "Should find all the words that exist on the Grid" $ do
      let found = findWords gwc languages
          asString = map (map cell2char) found
      asString `shouldBe` languages
    it "Should not find words that do not exist on the Grid" $ do
      findWords gwc ["FRENCH", "GERMAN", "ENGLISH"] `shouldBe` []
