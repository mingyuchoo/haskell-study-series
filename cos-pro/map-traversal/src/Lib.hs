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
  | null acc        = trace ("DEBUG 1> " <> show acc)  scanFwd [src] src
  | valueOfFwd /= 0 = trace ("DEBUG 3> " <> show acc)  scanFwd acc (x, y, newDirection d)
  | otherwise       = trace ("DEBUG 2> " <> show acc) $ do
      getFwd (x, y, d) : acc
  where
    valueOfFwd = getValueAt matrix $ getFwd (x, y, d)

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
getValueAt :: [[Int]] -- ^ matrix
        -> Coord   -- ^ postion for a cell
        -> Int     -- ^ value of the cell
getValueAt g (x, y, _) =
  g !! x !! y

-- |
-- https://stackoverflow.com/questions/5852722/replace-individual-list-elements-in-haskell
--
setValueAt :: [[Int]] -- ^ matrix
        -> Coord   -- ^ postion for a cell
        -> Int     -- ^ value of the cell
setValueAt g (x, y, _) = undefined

-- |
--
--
newDirection :: Int -- ^ current direction
             -> Int -- ^ next direction
newDirection x =
  mod (x + 3) 4
