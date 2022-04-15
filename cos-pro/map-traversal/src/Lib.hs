module Lib
    where


type Matrix = [[Int]]

data Coord = Coord { row       :: Int
                   , column    :: Int
                   , direction :: Int
                   }

instance Show Coord where
  show c@(Coord x y z) = show x <> show y <> show z
-- |
--
--
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


-- ----------------------------------------------------------------------------
getFirst :: Matrix -> Coord -> (Int, Coord)
getFirst m c@(Coord x y z) =
  head $ filter predicate values
  where
    values = valuesFrom m c
    predicate w = fst w == 0



valuesFrom :: Matrix -> Coord -> [(Int, Coord)]
valuesFrom m c =
  map fn (mkFwd c)
  where
    fn :: Coord -> (Int, Coord)
    fn c@(Coord x y d) = (m !! x !! y, c)


mkFwd :: Coord -> [Coord]
mkFwd (Coord x y d) =
  zipWith mkPos ps ds
  where
    ps = map (\w -> forward !! w) ds
    ds = directions d
    mkPos :: (Int, Int) -> Int -> Coord
    mkPos (u, v) = Coord (x+u) (y+v)

forward :: [(Int, Int)]
forward =
  [ (-1, 0) -- 0: North
  , ( 0, 1) -- 1: East
  , ( 1, 0) -- 2: South
  , ( 0,-1) -- 3: West
  ]

directions :: Int -> [Int]
directions = mkSeq 4 []
  where
    mkSeq :: Int -> [Int] -> Int -> [Int]
    mkSeq 0 l d = let rl = reverse l in last rl : init rl
    mkSeq n l d = mkSeq (n-1) (newDirection d:l) (newDirection d)

newDirection :: Int -> Int
newDirection x =
  mod (x + 3) 4

-- -----------------------------------------------------------------------------
checkBwd :: Coord -> Coord
checkBwd (Coord x y d) =
  Coord (x+u) (y+v) d
  where
    (u, v) = backward !! d
    backward =
      [ ( 1, 0)
      , ( 0,-1)
      , (-1, 0)
      , ( 0, 1)
      ]
