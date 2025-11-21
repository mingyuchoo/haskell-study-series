
module Lib
    ( Cell (Cell, Indent)
    , Game (gameGrid, gameWords)
    , Grid
    , cell2char
    , completed
    , fillInBlanks
    , findWord
    , findWordInCellLinePrefix
    , findWordInLine
    , findWords
    , formatGame
    , formatGrid
    , gridWithCoords
    , makeGame
    , makeRandomGrid
    , outputGrid
    , playGame
    , score
    , skew
    , totalWords
    , zipOverGrid
    , zipOverGridWith
    ) where

import           Data.Char     (toLower)
import           Data.Kind     (Type)
import           Data.List     (transpose)
import qualified Data.Map      as M
import           Data.Maybe    (catMaybes, listToMaybe)

import           Prelude       hiding (Word)

import           System.Random (Random (randomRs), RandomGen (split))

-- |
--
type Game :: Type
data Game = Game { gameGrid  :: Grid Cell
                 , gameWords :: M.Map String (Maybe [Cell])
                 }
     deriving (Show)
-- |
--
type Cell :: Type
data Cell = Cell (Int, Int) Char
          | Indent
     deriving (Eq, Ord, Show)

-- |
--
type Grid :: Type -> Type
type Grid a = [[a]]

-- |
--
type Word :: Type
type Word = String


-- |
--
makeGame :: Grid Char -> [String] -> Game
makeGame grid words =
  let gwc = gridWithCoords grid
      tuplify word = (word, Nothing)
      list = map tuplify words
      dict = M.fromList list
  in Game gwc dict

-- |
--
totalWords :: Game -> Int
totalWords game = length . M.keys  $ gameWords game

-- |
--
score :: Game -> Int
score game = length . catMaybes . M.elems  $ gameWords game

-- |
--
completed :: Game -> Bool
completed game = score game == totalWords game

-- |
--
playGame :: Game -> String -> Game
playGame game word | not $ M.member word (gameWords game) = game
playGame game word =
  let grid = gameGrid game
      foundWord = findWord grid word
  in case foundWord of
      Nothing -> game
      Just cs ->
        let dict = gameWords game
            newDict = M.insert word foundWord dict
        in game { gameWords = newDict }

-- |
--
formatGame :: Game -> String
formatGame game = formatGameGrid game
                  ++ "\n\n"
                  ++ (show $ score game)
                  ++ "/"
                  ++ (show $ totalWords game)

-- |
--
makeRandomGrid :: RandomGen t => t -> [[Char]]
makeRandomGrid gen =
  let (gen1, gen2) = split gen
      row = randomRs ('A', 'Z') gen1
  in row : makeRandomGrid gen2

-- |
--
fillInBlanks :: RandomGen a => a -> Grid Char -> Grid Char
fillInBlanks gen grid =
  let r = makeRandomGrid gen
      fill '_' r = r
      fill c _   = c
  in zipOverGridWith fill grid r

-- |
--
zipOverGrid :: Grid a -> Grid b -> Grid (a,b)
zipOverGrid = zipWith zip

-- |
--
zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith
-- /== zipOverGridWith f a b = (zipWith (zipWith f)) a b

-- |
--
coordsGrid :: Grid (Int, Int)
coordsGrid =
  let rows = map repeat [0..]
      cols = repeat [0..]
  in zipOverGrid rows cols

-- |
--
gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords = zipOverGridWith Cell coordsGrid

-- |
--
outputGrid :: Grid Cell -> IO ()
outputGrid = putStrLn . formatGrid

-- |
--
mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map

-- |
--
formatGameGrid :: Game -> String
formatGameGrid game =
  let grid = gameGrid game
      dict = gameWords game :: M.Map String (Maybe [Cell])
      cellSet = concat . catMaybes . M.elems $ dict
      formatCell cell =
        let char = cell2char cell
        in if cell `elem` cellSet then char else toLower char
      charGrid = mapOverGrid formatCell grid
  in unlines charGrid

-- |
--
formatGrid :: Grid Cell -> Word
formatGrid = unlines . mapOverGrid cell2char

-- |
--
cell2char :: Cell -> Char
cell2char (Cell _ c) = c
cell2char Indent     =  '?'

-- |
--
getLines :: Grid Cell -> [[Cell]]
getLines grid =
  let horizontal = grid
      vertical = transpose grid
      diagonal1 = diagonalize grid
      diagonal2 = diagonalize (map reverse grid)
      lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
  in lines ++ map reverse lines

-- |
--
diagonalize :: Grid Cell -> Grid Cell
diagonalize = transpose . skew

-- |
--
skew :: Grid Cell -> Grid Cell
skew [] = []
skew (l:ls) = l : skew (map indent ls)
  where
    indent line = Indent : line

-- |
--
findWord :: Grid Cell -> Word -> Maybe [Cell]
findWord grid word =
  let lines = getLines grid
      foundWords = map (findWordInLine word) lines
  in listToMaybe (catMaybes foundWords)

-- |
--
findWords :: Grid Cell -> [Word] -> [[Cell]]
findWords grid words =
  let foundWords = map (findWord grid) words
  in catMaybes foundWords

-- |
--
findWordInLine :: Word -> [Cell] -> Maybe [Cell]
findWordInLine _ [] = Nothing
findWordInLine word line =
  let found = findWordInCellLinePrefix [] word line
  in case found of
    Nothing     -> findWordInLine word (tail line)
    cs@(Just _) -> cs

-- |
--
findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellLinePrefix acc (x:xs) (c:cs) | x == cell2char c = findWordInCellLinePrefix (c:acc) xs cs
findWordInCellLinePrefix acc [] _                             = Just $ reverse acc
findWordInCellLinePrefix  _ _ _                               = Nothing
