module Lib
    where

import           Data.List (sort, transpose)
import           Flow      ((<|))

someFunc :: IO ()
someFunc = do
  print <| solution1 matrix1
  print <| solution1 matrix2

-- I want to get the largest number
-- from the result of
-- picking the smallest number in each row.

-- M: the number of columns
-- N: the number of rows
-- N x M: matrix of numbers


-- |
--
--
n1 :: Int
n1 = 3

m1 :: Int
m1 = 3

matrix1 :: [[Int]]
matrix1 = [[3,1,2],[4,1,4],[2,2,2]]

answer1 :: Int
answer1 = 2


-- |
--
--
n2 :: Int
n2 = 2

m2 :: Int
m2 = 4

matrix2 :: [[Int]]
matrix2 = [[27,3,1,8],[3,3,3,4]]

answer2 :: Int
answer2 = 3


-- |
--
--
solution1 :: [[Int]] -> Int
solution1 matrix =
  last <| head <| transpose <| map sort matrix


-- |
--
--
solution2 :: [[Int]] -> Int
solution2 matrix =
  foldl max 0 <| getMin matrix
  where
    getMin = map (foldl min 99999)
