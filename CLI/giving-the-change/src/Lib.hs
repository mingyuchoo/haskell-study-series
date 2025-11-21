{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib
    where

-- |
--
calc :: (Integral a) => a -> [a]  -> a -> a
calc 0 _  z = z + 0
calc _ [] z = z + 0
calc x y  z
    | div x (last y) /= 0 = calc (mod x (last y)) (init y) (z + div x (last y))
    | div x (last y) == 0 = calc x (init y) z

-- |
--
driver :: (Integral a) => a -> a
driver x =
    calc x  [10, 50, 100, 500, 1000, 5000, 10000, 50000] 0

-- |
--
someFunc :: IO ()
someFunc = do
    putStrLn "Input price to be changed"

    input <- readLn

    print (driver input::Int)
