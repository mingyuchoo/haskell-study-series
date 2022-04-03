module Lib
    where

import           Debug.Trace
import           System.IO   (BufferMode (NoBuffering), hSetBuffering, stdout)


someFunc :: IO ()
someFunc = do
  hSetBuffering stdout NoBuffering
  putStr "Enter the amount to be changed: (ex. 1260) "
  num <- getLine
  print . calc1 0 $ read num
  print . calc2 0 $ read num

-- Given 500, 100, 50, 10 units coins
-- When to give a customer change
-- I want to give change back to the smallest number of coins

-- |
--
--
calc1 :: Int -> Int -> Int
calc1 x y =
  if y >= 500 then calc1 (x + 1) (y - 500) else
  if y >= 100 then calc1 (x + 1) (y - 100) else
  if y >=  50 then calc1 (x + 1) (y -  50) else
  if y >=  10 then calc1 (x + 1) (y -  10) else x


-- |
--
--
calc2 :: Int -> Int -> Int
calc2 x y
  | y >= 500 = calc2 (x + 1) (y - 500)
  | y >= 100 = calc2 (x + 1) (y - 100)
  | y >=  50 = calc2 (x + 1) (y -  50)
  | y >=  10 = calc2 (x + 1) (y -  10)
  | otherwise = x
