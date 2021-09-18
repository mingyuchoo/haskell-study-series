module Lib where
-------------------------------------------------------------------------------
-- $ ./parallel +RTS -N4
-- primes: 78498
-------------------------------------------------------------------------------
import Data.Int
import Control.Parallel
import Control.Parallel.Strategies
-------------------------------------------------------------------------------
-- 소수 판정

isPrime :: Int32 -> Bool
isPrime x = all (\n -> x `mod` n /= 0) [2..toEnum (floor $ sqrt $ fromIntegral x)]
-------------------------------------------------------------------------------
-- 2부터 1000000까지 수

arr :: [Int32]
arr = [2..1000000]
-------------------------------------------------------------------------------
-- 계산을 적용하면서 집계하는 병렬 계산 패턴

reduceP :: (b -> a) -> (a -> a -> a) -> [b] -> a
reduceP f _ [x] = f x
reduceP f (<+>) xs =
    (ys `par` zs) `pseq` (ys <+> zs)
  where
    len = length xs
    (ys',zs') = splitAt (len `div` 2) xs
    ys = reduceP f (<+>) ys'
    zs = reduceP f (<+>) zs'
-------------------------------------------------------------------------------
someFunc :: IO ()
someFunc = do
    -- 판정하면서 집계
    let primes = reduceP (fromEnum . isPrime) (+) arr
    -- 집계 결과를 표시
    putStr "primes: " >> print primes
-------------------------------------------------------------------------------
