{-# LANGUAGE StandaloneKindSignatures #-}
module Lib
    where

import           Control.Lens (element, (&), (.~))
import           Data.Maybe   (mapMaybe)

someFunc :: IO ()
someFunc = print driver

-- -----------------------------------------------------------------------------
-- 1. 사방 탐색하기
-- 2. 방문하기
-- 3. 범위 밖이면 무시하기
-- 4. 방문 기록하기
-- 5. 모든 element 스캔하기
-- -----------------------------------------------------------------------------

type Matrix :: *
type Matrix = [[Int]]

matrix :: Matrix
matrix = [ [0,0,1]
         , [0,1,0]
         , [1,0,1]
         ]

type Coordinate :: *
type Coordinate = (Int, Int)
-- -----------------------------------------------------------------------------
-- 1. 사방 탐색하기
-- 3. 범위 밖이면 무시하기

getPos :: Matrix -> Coordinate -> [Coordinate]
getPos m p@(i,j) =
    mapMaybe func dirs
  where
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

-- -----------------------------------------------------------------------------
-- 5. 모든 element 스캔하기

type Accumulate :: *
type Accumulate = (Matrix, Int)

driver :: Accumulate
driver =
  foldl fun (matrix, 0) [(i,j) | i <- [0..2], j <- [0..2]]
  where
    fun :: Accumulate -> (Int, Int) -> Accumulate
    fun acc@(m, c) p =
      if result
        then (newM, c+1)
        else (newM, c)
      where
        (newM, result) = travel m p

-- -----------------------------------------------------------------------------
-- 2. 방문하기

travel :: Matrix -> (Int, Int) -> (Matrix, Bool)
travel m p = visit' (m, False) [p]

visit' :: (Matrix, Bool) ->  [Coordinate] -> (Matrix, Bool)
visit' v []            = v
visit' v@(m, b) (c:cs) = visit' (visit' (visitMatrix v c) (getPos m c)) cs

-- -----------------------------------------------------------------------------
-- 4. 방문 기록하기

visitMatrix :: (Matrix, Bool) -> Coordinate -> (Matrix, Bool)
visitMatrix v@(m,b) (x,y) =
  case m !! x !! y of
    0 -> (m & element x . element y .~ 2, True)
    _ -> (m, False)
