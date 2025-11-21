module Lib
    ( someFunc
    ) where

import           Data.Kind (Type)

-- |
--
someFunc :: IO ()
someFunc = do
  putStrLn "--------------------"

  print $ fmap (+ 1) [-1, -3, 4, -12]
  print $ map (+ 1) [-1, -3, 4, -12]

  return ()
