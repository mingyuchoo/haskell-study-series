module Main where

-- | doubleMe
--
-- >>> doubleMe 0
-- 0
-- >>> doubleMe 1
-- 2
-- >>> doubleMe 2
-- 4
-- >>> doubleMe 100
-- 200
--

doubleMe :: Int -> Int
doubleMe x = x + x

main :: IO ()
main = putStrLn (show (doubleMe 3))
-- main = putStrLn "Hello, Haskell!"
