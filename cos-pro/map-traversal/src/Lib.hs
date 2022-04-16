module Lib
    where

import           Control.Lens (element, (&), (.~))

type Direct = Int
type Value = Int
type Row = Int
type Col = Int
type Matrix = [[Int]]
data Coord = Coord { row       :: Row
                   , column    :: Col
                   , direction :: Direct
                   }

instance Show Coord where
  show c@(Coord x y z) =
    "<" <> show x <>
    "," <> show y <>
    "," <> show z <>
    ">"

someFunc :: IO ()
someFunc =
  putStrLn "Hello, World"

start :: Coord
start = Coord 1 1 0

matrix :: Matrix
matrix =
  [ [1,1,1,1]
  , [1,0,0,1]
  , [1,1,0,1]
  , [1,1,1,1]
  ]

xMatrix :: Matrix
xMatrix =
  [[1,1,1,1],[1,2,2,1],[1,1,0,1],[1,1,1,1]]

yValue :: Value
yValue =
  0

zCoord :: Coord
zCoord =
  Coord 2 2 2

driver :: (Matrix, Value, Coord)
driver =
  moveFwd (xMatrix, yValue, zCoord)
-- ----------------------------------------------------------------------------
moveFwd :: (Matrix, Value, Coord) -> (Matrix, Value, Coord)
moveFwd (m, _, c@(Coord x y d)) =
    (visit m c, v, n)
  where
    (v, n) = filterFwd m c

filterFwd :: Matrix -> Coord -> (Value, Coord)
filterFwd m c =
  let
    predicate w = fst w == 0
    values = valuesFrom mkFwd m c
  in
    case filter predicate values of
      [] -> filterBwd m c
      vs -> head vs

filterBwd :: Matrix -> Coord -> (Value, Coord)
filterBwd m c =
  let
    predicate w = fst w /= 1
    values = valuesFrom mkBwd m c
  in
    case filter predicate values of
      [] -> (0, Coord 0 0 0)
      vs -> head vs

valuesFrom :: (Coord -> [Coord]) -> Matrix -> Coord -> [(Value, Coord)]
valuesFrom f m c =
  map fn (f c)
  where
    fn :: Coord -> (Value, Coord)
    fn c@(Coord x y d) = (m !! x !! y, c)

mkFwd :: Coord -> [Coord]
mkFwd (Coord x y d) =
  zipWith mkPos ps ds
  where
    ps = map (\w -> forward !! w) ds
    ds = directions d

    mkPos :: (Row, Col) -> Direct -> Coord
    mkPos (u, v) = Coord (x+u) (y+v)

    forward :: [(Row, Col)]
    forward =
      [ (-1, 0) -- 0: North
      , ( 0, 1) -- 1: East
      , ( 1, 0) -- 2: South
      , ( 0,-1) -- 3: West
      ]

mkBwd :: Coord -> [Coord]
mkBwd (Coord x y d) =
  zipWith mkPos ps ds
  where
    ps = map (\w -> backward !! w) ds
    ds = directions d

    mkPos :: (Row, Col) -> Direct -> Coord
    mkPos (u, v) = Coord (x+u) (y+v)

    backward :: [(Row, Col)]
    backward =
      [ ( 1, 0) -- 0: North
      , ( 0,-1) -- 1: East
      , (-1, 0) -- 2: South
      , ( 0, 1) -- 3: West
      ]

directions :: Direct -> [Direct]
directions = mkSeq 4 []
  where
    mkSeq :: Direct -> [Direct] -> Direct -> [Direct]
    mkSeq 0 l d = let rl = reverse l in last rl : init rl
    mkSeq n l d = mkSeq (n-1) (newDirection d:l) (newDirection d)

newDirection :: Direct -> Direct
newDirection x =
  mod (x + 3) 4

-- -----------------------------------------------------------------------------

visit :: Matrix -> Coord -> Matrix
visit m c@(Coord x y _) = m & element x . element y .~ 2

