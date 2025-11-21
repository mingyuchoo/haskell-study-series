module Lib
    where

import           Data.List

someFunc :: IO ()
someFunc = print $ solution list


-- Given a list of `N` numbers
-- and with the following constrains
--  - can add numbers `M` times
--  - can use same item `K` times in a row
-- When generate a sum value with the list
-- and the constrains
-- Then I should get the biggest sum value.

-- |
--
--
list :: [Int]
list = [2,4,5,4,6]

n :: Int
n = length list

m :: Int
m = 8

k :: Int
k = 3


-- |
--
--
solution :: [Int] -> Int
solution xs = (first xs * k * quotient) + (second xs * remainder)
  where
    rsort :: [Int] -> Int -> Int
    rsort l x = (reverse . sort) l !! x

    first :: [Int] -> Int
    first l = rsort l 0

    second :: [Int] -> Int
    second l = rsort l 1

    quotient :: Int
    quotient = div m k

    remainder :: Int
    remainder = mod m k
