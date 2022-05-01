module Lib
    where

import           Control.Lens (element, (&), (.~))
import           Data.Maybe   (mapMaybe)
someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- -----------------------------------------------------------------------------
-- 1. 사방 탐색하기
-- 2. 방문하기
-- 3. 범위 밖이면 무시하기
-- 4. 방문 기록하기
-- 5. 모든 element 스캔하기
-- -----------------------------------------------------------------------------

matrix :: [[Int]]
matrix = [ [0,0,1]
         , [0,1,0]
         , [1,0,1]
         ]

-- -----------------------------------------------------------------------------
-- 1. 사방 탐색하기
-- 3. 범위 밖이면 무시하기

dirs :: [(Int, Int)]
dirs = [ ( 0,-1) -- North
       , ( 0, 1) -- South
       , (-1, 0) -- West
       , ( 1, 0) -- East
       ]

newPos :: Int -> Int -> [Int]
newPos i j =
  let
    func (x, y)
      | (i+x) < 0 = Nothing
      | (j+y) < 0 = Nothing
      | otherwise = Just $ matrix !! (i+x) !!(j+y)
  in
    mapMaybe func dirs

-- -----------------------------------------------------------------------------
-- 4. 방문 기록하기

newMatrix :: Int -> Int -> Int -> [[Int]]
newMatrix x y z = matrix & element x . element y .~ z

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

