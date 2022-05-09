module Lib
    where

import           Control.Lens (element, (&), (.~))
import           Data.Maybe   (mapMaybe)
import           Debug.Trace

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- -----------------------------------------------------------------------------
-- 1. 사방 탐색하기
-- 2. 방문하기
-- 3. 범위 밖이면 무시하기
-- 4. 방문 기록하기
-- 5. 모든 element 스캔하기
-- -----------------------------------------------------------------------------

type Matrix = [[Int]]

matrix :: Matrix
matrix = [ [0,0,1]
         , [0,1,0]
         , [1,0,1]
         ]

type Coordinate = (Int, Int)
-- -----------------------------------------------------------------------------
-- 1. 사방 탐색하기
-- 3. 범위 밖이면 무시하기

getPos :: Matrix -> Coordinate -> [Coordinate]
getPos m p@(i,j) =
  let
    rows = 3
    cols = 3
    dirs :: [Coordinate]
    dirs = [ ( 0,-1) -- North
           , ( 0, 1) -- South
           , (-1, 0) -- West
           , ( 1, 0) -- East
           ]
    func (x, y)
      | (i+x) < 0 || (i+x) >= rows = Nothing
      | (j+y) < 0 || (j+y) >= cols = Nothing
      | otherwise = case m !! (i+x) !! (j+y) of
          0 -> Just (i+x, j+y)
          _ -> Nothing
  in
    mapMaybe func dirs

-- -----------------------------------------------------------------------------
-- 2. 방문하기

drive :: Matrix
drive = visit' (matrix, True) [(0,0)]

visit' :: Matrix ->  [Coordinate] -> Matrix
visit' m _      = m
visit' m []     = m
visit' m (c:cs) = visit' (visit' (visitMatrix m c) (getPos m c)) cs

-- -----------------------------------------------------------------------------
-- 4. 방문 기록하기

visitMatrix :: Matrix -> Coordinate -> Matrix
visitMatrix m (x,y) =
  case m !! x !! y of
    0 -> m & element x . element y .~ 2
    _ -> m
-- -----------------------------------------------------------------------------
-- 5. 모든 element 스캔하기

a :: [[String]]
a =  fmap (\x -> fmap (\y -> show y) x) matrix

a' :: IO [[()]]
a' = mapM (\x -> mapM (\y -> print y) x) matrix

b :: String
b = matrix >>= (\x -> x >>= (\y -> show y))

b' :: String
b' = do
  x <- matrix
  y <- x
  show y


c :: [String]
c = do
  x <- matrix
  y <- x
  return <$> show y

