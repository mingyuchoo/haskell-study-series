module Lib
    where

-- |
--
someFunc :: IO ()
someFunc = putStrLn "someFunc"

{-
N (2 <= N <= 100,000)
K (2 <= K <= 100,000)

OP1) N - 1
OP2) N / K

I want to get the smallest number of operation.

Given N = 25, K = 5
When run solution
Then it should be 2
-}

-- |
--
solution :: Int -> Int -> Int
solution = calc 0
  where
    calc :: Int -> Int -> Int -> Int
    calc _   _ 0 = error "Error: divided by 0"
    calc acc 1 _ = acc
    calc acc x y | x `mod` y == 0 = calc (acc + 1) (x `div` y) y
                 | otherwise      = calc (acc + 1) (x - 1) y
