module Lib
    where

import           Debug.Trace

type Matrix = [[Int]]

type Coord =
  ( Int -- ^ row
  , Int -- ^ column
  , Int -- ^ direction
  )

-- |
--
--
someFunc :: IO ()
someFunc =
  putStrLn "Hello, World"

size :: (Int, Int)
size =
  (4,4)

start :: Coord
start =
  (1,1,0)

matrix :: Matrix
matrix =
  [ [1,1,1,1]
  , [1,0,0,1]
  , [1,1,0,1]
  , [1,1,1,1]
  ]


checkNextDirection :: Matrix -> Coord -> (Int, Int)
checkNextDirection m c@(x, y, _) =
  let
    next = newDirection $ snd $ checkDirection m c
  in
    checkDirection m (x, y, next)

-- -----------------------------------------------------------------------------
-- |
--
--
checkDirection :: Matrix -> Coord -> (Int, Int)
checkDirection m c =
  getValueAt m $ getFwd c

getValueAt :: Matrix -> Coord -> (Int, Int)
getValueAt m (x, y, d) =
  (m !! x !! y, d)

getFwd :: Coord -> Coord
getFwd (x, y, d) =
  (x+u, y+v, d)
  where
    (u, v) = forward !! d
    forward =
      [ (-1, 0)
      , ( 0, 1)
      , ( 1, 0)
      , ( 0,-1)
      ]
-- -----------------------------------------------------------------------------

checkBwd :: Coord -> Coord
checkBwd (x, y, d) =
  (x+u, y+v, d)
  where
    (u, v) = backward !! d
    backward =
      [ ( 1, 0)
      , ( 0,-1)
      , (-1, 0)
      , ( 0, 1)
      ]

dirs :: Int -> [Int]
dirs = mkSeq 4 []
  where
    mkSeq :: Int -> [Int] -> Int -> [Int]
    mkSeq 0 l d = d : reverse l
    mkSeq n l d = mkSeq (n-1) (newDirection d:l) (newDirection d)



newDirection :: Int -> Int
newDirection x =
  mod (x + 3) 4
