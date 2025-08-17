module Lib
    where

import           Flow ((<|))

-- |
--
--
someFunc :: IO ()
someFunc = do
  putStr "Input hour to find out including the number 3> "
  number <- getLine
  print <| solution1 (read number)
  print <| solution2 (read number)

-- |
--
--
solution1 :: Int -> Int
solution1 n =
  length <| find [ hour <> minute <> second
                 | hour   <- hours n
                 , minute <- minutes
                 , second <- seconds
                 ]

  where
    hours :: Int -> [String]
    hours x = map show [0..x]

    minutes :: [String]
    minutes = map show [0..59]

    seconds :: [String]
    seconds = map show [0..59]

    find :: [String] -> [String]
    find = filter (elem '3')


solution2 :: Int -> Int
solution2 x = ((x+1) - nums x) * ((60-15)*15 + 15*60) + (nums x * 60 * 60)
  where
    nums :: Int -> Int
    nums h = length <| filter (elem '3') <| map show [0..h]
