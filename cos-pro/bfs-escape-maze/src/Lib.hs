{-# LANGUAGE StandaloneKindSignatures #-}
module Lib
    where

import           Control.Lens (element, (&), (.~))
import           Data.Kind    (Type)
import           Data.Maybe   (mapMaybe)
import           Flow         ((<|))

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- -----------------------------------------------------------------------------
-- 1. 방문하기 (deque)
-- 2. 방문 기록하기 (현까지 누적한 걸음 수 + 1)
-- 3. 사방 탐색하기 (북, 남, 서, 동)
-- 4. 범위 밖이면 무시하기
-- 5. 방문할 기록으로 남기기 (enque)
-- 6. 방문할 기록이 없을 때 까지 1 부터 진행하기
-- -----------------------------------------------------------------------------

type Coordinate :: Type
type Coordinate = (Int, Int)

type Accumulate :: Type
type Accumulate = (Matrix, Int)

type Matrix :: Type
type Matrix = [[Int]]

matrix1 :: Matrix
matrix1 = [ [1,1,0]
          , [0,1,0]
          , [0,1,1]
          ]

matrix2 :: Matrix
matrix2 = [ [1,0,1,0,1,0]
          , [1,1,1,1,1,1]
          , [0,0,0,0,0,1]
          , [1,1,1,1,1,1]
          , [1,1,1,1,1,1]
          ]

-- -----------------------------------------------------------------------------
-- 6. 방문할 기록이 없을 때 까지 1 부터 진행하기

driver :: Matrix -> Accumulate
driver m = undefined

-- -----------------------------------------------------------------------------
-- 1. 방문하기
-- 5. 방문할 기록으로 남기기 (enque)

starter :: Matrix -> (Int, Int) -> (Matrix, Int)
starter m p = visit (m, 1) [p]

visit :: (Matrix, Int) -> [Coordinate] -> (Matrix, Int)
visit v []            = v
visit v@(m, n) (c:cs) = undefined

-- -----------------------------------------------------------------------------
-- 2. 방문 기록하기

visitMatrix :: (Matrix, Int) -> Coordinate -> (Matrix, Int)
visitMatrix (m,n) (x,y) =
  case m !! x !! y of
    0 -> (m & element x . element y .~ 2, n+1)
    _ -> (m,n)

-- -----------------------------------------------------------------------------
-- 3. 사방 탐색하기 (북,남,서,동)
-- 4. 범위 밖이면 무시하기

getPos :: (Int, Int) -> Matrix -> Coordinate -> [Coordinate]
getPos (row,col) m (i,j) =
    mapMaybe func dirs
  where
    dirs :: [Coordinate]
    dirs = [ ( 0,-1) -- North
           , ( 0, 1) -- South
           , (-1, 0) -- West
           , ( 1, 0) -- East
           ]
    func (x,y)
      | (i+x) < 0 || (i+x) >= row = Nothing
      | (j+y) < 0 || (j+y) >= col = Nothing
      | otherwise = case m !! (i+x) !! (j+y) of
          0 -> Just (i+x, j+y)
          _ -> Nothing

