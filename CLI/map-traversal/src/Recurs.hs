module Recurs
    where

import           Debug.Trace (trace)

-- |
--
func1 :: Int -> Int ->Int
func1 x y =
  case x of
    0 -> trace ("DEBUG 1> x: " <> show x <> ", y:" <> show y) y
    _ -> trace ("DEBUG 2> x: " <> show x <> ", y:" <> show y) func1 (x - 1) (y + 1)

-- |
--
func2 :: Int -> Int ->Int
func2 x y | x == 0    = trace ("DEBUG 1> x: " <> show x <> ", y:" <> show y) y
          | otherwise = trace ("DEBUG 2> x: " <> show x <> ", y:" <> show y) func2 (x - 1) (y + 1)

-- |
--
func3 :: Int -> Int ->Int
func3 0 y = trace ("DEBUG 1> x: " <> show 0 <> ", y:" <> show y) y
func3 x y = trace ("DEBUG 2> x: " <> show x <> ", y:" <> show y) func3 (x - 1) (y + 1)
