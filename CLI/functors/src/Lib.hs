module Lib
  ( someFunc,
  )
where

import Data.Kind (Type)

someFunc :: IO ()
someFunc = do
  _ <- putStrLn "--------------------"
  _ <- print $ fmap (+ 1) [-1, -3, 4, -12]
  _ <- print $ map (+ 1) [-1, -3, 4, -12]
  return ()
