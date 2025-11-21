module Lib
    ( someFunc
    ) where

import           Data.Kind (Type)
someFunc :: IO ()
someFunc = do
  putStr "Please enter a number: "
  i <- read <$> getLine
  putStrLn . show . square $ i

{-# INLINE square #-}
square :: Int -> Int
square x = x * x
