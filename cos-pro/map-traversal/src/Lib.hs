module Lib
    where

import           Debug.Trace

-- |
--
--
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

-- |
--
--
size :: (Int, Int) -- ^ size of matrix
size =
  (4,4)

-- |
--
--
init :: Coord -- ^ character information
init =
  (1,1,0)

-- |
--
--
matrix :: [[Int]] -- ^ values for matrix
matrix =
  [ [1,1,1,1]
  , [1,0,0,1]
  , [1,1,0,1]
  , [1,1,1,1]
  ]

scanFwd :: [Coord] -- ^ accumulate
        -> Coord   -- ^ source position
        -> [Coord] -- ^ target position
scanFwd acc src@(x, y, d)
  | valueOfFwd == 0 = trace "DEBUG 1> " getFwd (x, y, d) : acc
  | null acc        = trace "DEBUG 2> " scanFwd [src] src
  | otherwise       = trace "DEBUG 3> " scanFwd acc (x, y, newDirection d)
  where
    valueOfFwd = cellAt matrix $ getFwd (x, y, d)

-- |
--
--
getFwd :: Coord -- ^ character information
       -> Coord -- ^ value of forward position
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

-- |
--
--
checkBwd :: Coord -- ^ character information
         -> Coord -- ^ value of forward position
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


-- |
--
--
cellAt :: [[Int]] -- ^ matrix
       -> Coord   -- ^ postion for a cell
       -> Int     -- ^ value of the cell
cellAt g (x, y, _) =
  g !! x !! y

-- |
--
--
newDirection :: Int -- ^ current direction
             -> Int -- ^ next direction
newDirection x =
  mod (x + 3) 4
