module Lib
    where

import qualified Data.Map   as M
import           Data.Maybe (mapMaybe)

import           Flow       ((<|))

type Position = (Int, Int)

-- |
--
--
someFunc :: IO ()
someFunc = do
  putStr "Input initial position> "
  position <- getLine
  print <| find <| conv position

-- |
--
--
find :: Position -- ^ initial posion of the knight
     -> Int      -- ^ the number of moveable positions
find (x, y) = length <| mapMaybe (possible . add (x, y)) movement

-- |
--
--
possible :: Position       -- ^ postion
         -> Maybe Position -- ^ possible position
possible (x, y)
  | x <= 0 || x >= 8  = Nothing
  | y <= 0 || y >= 8  = Nothing
  | otherwise         = Just (x, y)

-- |
--
--
add :: Position -- ^ initial postion
    -> Position -- ^ relative postion
    -> Position -- ^ target postion
add (x, y) (u, v) = (x+u, y+v)

-- |
--
--
movement :: [Position]
movement = [ (-1, -2)
           , ( 1, -2)
           , ( 2, -1)
           , ( 2,  1)
           , ( 1,  2)
           , (-1,  2)
           , (-2,  1)
           , (-2, -1)
           ]

-- |
--
--
conv :: String   -- ^ input string
     -> Position -- ^ converted postion value
conv s | length s /= 2 = error "error: not correct length"
       | otherwise =
           case s of
             (x:xs) -> (aton x, read xs)
             _      -> (0, 0)

-- |
--
--
aton :: Char -- ^ a character
     -> Int  -- ^ the integer value mapped with the character
aton x =
  M.findWithDefault 0 x (M.fromList matrix)
  where
    matrix = zip ['a'..'h'] [1..]
