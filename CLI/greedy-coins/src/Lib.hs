module Lib
    where

import qualified Data.Map    as M

import           Debug.Trace ()

import           System.IO   (BufferMode (NoBuffering), hSetBuffering, stdout)

-- |
--
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

type Change = Int
type Coin = Int
type Pack = (Coin, Int)

-- |
--
calc1 :: Change -> Coin -> Int
calc1 x y =
  if y >= 500 then calc1 (x + 1) (y - 500) else
  if y >= 100 then calc1 (x + 1) (y - 100) else
  if y >=  50 then calc1 (x + 1) (y -  50) else
  if y >=  10 then calc1 (x + 1) (y -  10) else x


-- |
--
calc2 :: Change -> Coin -> Int
calc2 x y
  | y >= 500  = calc2 (x + 1) (y - 500)
  | y >= 100  = calc2 (x + 1) (y - 100)
  | y >=  50  = calc2 (x + 1) (y -  50)
  | y >=  10  = calc2 (x + 1) (y -  10)
  | otherwise = x


-- |
--
calc3 :: Change -> [Coin] -> Int
calc3 change coins =
  foldl reducer 0 $ func1 change coins []

reducer :: Int -> Pack -> Int
reducer x y = x + snd y

func1 :: Change -> [Coin] -> [Pack] -> [Pack]
func1 0 _  packs = packs
func1 _ [] packs = packs
func1 x (y:ys) packs
  | quotient > 0 = func1 remain ys ((y, quotient):packs)
  | otherwise    = func1 x ys packs
  where
    quotient = x `div` y
    remain = x - y * quotient


-- |
--
calc4 :: Change -> [Coin] -> Int
calc4 change coins =
  sum $ M.elems $ func2 change coins M.empty

func2 :: Change -> [Coin] -> M.Map Coin Int -> M.Map Coin Int
func2 0 _  m = m
func2 _ [] m = m
func2 x (y:ys) m
  | quotient > 0 = func2 remain ys (M.insert y quotient m)
  | otherwise    = func2 x ys m
  where
    quotient = x `div` y
    remain = x - y * quotient
