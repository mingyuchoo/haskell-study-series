module Lib
    where

import           Control.Lens (element, (&), (.~))

import           Data.Kind    (Type)
import           Data.Maybe   (mapMaybe)

type Direct :: Type
type Direct = Int

type Value :: Type
type Value = Int

type Row :: Type
type Row = Int

type Col :: Type
type Col = Int

type Matrix :: Type
type Matrix = [[Int]]

type Coord :: Type
data Coord = Coord { row       :: Row
                   , column    :: Col
                   , direction :: Direct
                   }

instance Show Coord where
  show (Coord x y d) =
    "<" <> show x <>
    "," <> show y <>
    "," <> show d <>
    ">"

-- |
--
someFunc :: IO ()
someFunc =
  print $ moveFwd (matrix, direction start, start) 0

-- |
--
start :: Coord
start =
  Coord 1 1 0

-- |
--
matrix :: Matrix
matrix =
  [ [1,1,1,1]
  , [1,0,0,1]
  , [1,1,0,1]
  , [1,1,1,1]
  ]

-- |
--
moveFwd :: (Matrix, Value, Coord) -> Int -> Int
moveFwd (m, _, c) acc
  | v == -1 = acc
  | otherwise = moveFwd (visit m c, v, n) (acc + 1)
  where
    (v, n) = filterFwd m c

-- |
--
filterFwd :: Matrix -> Coord -> (Value, Coord)
filterFwd m c =
  let
    predicate w = fst w == 0
    values = valuesFrom mkFwd m c
  in
    case filter predicate values of
      [] -> filterBwd m c
      vs -> head vs

-- |
--
filterBwd :: Matrix -> Coord -> (Value, Coord)
filterBwd m c@(Coord _ _ d) =
  let
    values = valuesFrom mkBwd m c
  in
    case mapMaybe (canGoBack d) values of
      [] -> (-1, Coord (-1)(-1)(-1))
      vs -> head vs

-- |
--
canGoBack :: Direct -> (Value, Coord) -> Maybe (Value, Coord)
canGoBack d c@(v, Coord _ _ z)
  | d == z && v /= 1 = Just c
  | otherwise = Nothing

-- |
--
valuesFrom :: (Coord -> [Coord]) -> Matrix -> Coord -> [(Value, Coord)]
valuesFrom f m c =
  map fn (f c)
  where
    fn :: Coord -> (Value, Coord)
    fn a@(Coord x y _) = (m !! x !! y, a)

-- |
--
mkFwd :: Coord -> [Coord]
mkFwd (Coord x y d) =
  zipWith mkPos ps ds
  where
    ps = map (\w -> forward !! w) ds
    ds = directions d

    mkPos :: (Row, Col) -> Direct -> Coord
    mkPos (u, v) = Coord (x + u) (y + v)

    forward :: [(Row, Col)]
    forward =
      [ (-1, 0) -- 0: North
      , ( 0, 1) -- 1: East
      , ( 1, 0) -- 2: South
      , ( 0,-1) -- 3: West
      ]

-- |
--
mkBwd :: Coord -> [Coord]
mkBwd (Coord x y d) =
  zipWith mkPos ps ds
  where
    ps = map (\w -> backward !! w) ds
    ds = directions d

    mkPos :: (Row, Col) -> Direct -> Coord
    mkPos (u, v) = Coord (x + u) (y + v)

    backward :: [(Row, Col)]
    backward =
      [ ( 1, 0) -- 0: North
      , ( 0,-1) -- 1: East
      , (-1, 0) -- 2: South
      , ( 0, 1) -- 3: West
      ]

-- |
--
directions :: Direct -> [Direct]
directions =
  mkSeq 4 []
  where
    mkSeq :: Direct -> [Direct] -> Direct -> [Direct]
    mkSeq 0 l _ = let rl = reverse l in last rl : init rl
    mkSeq n l d = mkSeq (n - 1) (newDirection d : l) (newDirection d)

-- |
--
newDirection :: Direct -> Direct
newDirection x =
  mod (x + 3) 4

-- |
--
visit :: Matrix -> Coord -> Matrix
visit m (Coord x y _) =
  m & element x . element y .~ 2
