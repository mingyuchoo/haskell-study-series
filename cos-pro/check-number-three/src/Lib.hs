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
  print . length <| find [ hour <> minute <> second
                         | hour <- hours (read number)
                         , minute <- minutes
                         , second <- seconds
                         ]


-- |
--
--
hours :: Int -> [String]
hours x = map show [0..x]

-- |
--
--
minutes :: [String]
minutes = map show [0..59]

-- |
--
--
seconds :: [String]
seconds = map show [0..59]

-- |
--
--
find :: [String] -> [String]
find = filter (elem '3')
