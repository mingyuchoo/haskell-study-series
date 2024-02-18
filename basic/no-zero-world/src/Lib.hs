module Lib
    ( someFunc
    ) where

import           Flow ((<|))

someFunc :: IO ()
someFunc = do
  print <| converter 9949999 1
  return ()

converter :: Int -> Int -> Int
converter x1 x2 =
  read <| concatMap replaceZero . show <| x1 + x2
  where
    replaceZero :: Char -> [Char]
    replaceZero '0' = ['1']
    replaceZero c   = [c]
