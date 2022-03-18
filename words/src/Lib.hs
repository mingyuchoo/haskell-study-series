module Lib
    ( findWord
    , findWordInLine
    , findWords
    , formatGrid
    , outputGrid
    , skew
    ) where

import           Data.List  (isInfixOf, transpose)
import           Data.Maybe (catMaybes)
import           Prelude    hiding (Word)

type Grid = [String]
type Word = String

outputGrid :: Grid -> IO ()
outputGrid grid = putStrLn $ formatGrid grid

formatGrid :: Grid -> Word
formatGrid = unlines


getLines :: Grid -> [String]
getLines grid =
  let horizontal = grid
      vertical = transpose grid
      diagonal1 = diagonalize grid
      diagonal2 = diagonalize (map reverse grid)
      lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
  in lines ++ (map reverse lines)

diagonalize :: Grid -> Grid
diagonalize = transpose . skew

skew :: Grid -> Grid
skew [] = []
skew (l:ls) = l : skew (map indent ls)
  where
    indent line = '_' : line

findWord :: Grid -> Word -> Maybe Word
findWord grid word =
  let lines = getLines grid
      found = or $ map (findWordInLine word) lines
  in if found then Just word else Nothing

findWords :: Grid -> [Word] -> [Word]
findWords grid words =
  let foundWords = map (findWord grid) words
  in catMaybes foundWords

findWordInLine :: Word -> String -> Bool
findWordInLine = isInfixOf

