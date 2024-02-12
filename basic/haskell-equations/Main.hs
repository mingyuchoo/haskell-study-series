module Main
  where

import Control.Monad (mapM_)

-- When 10 kg is removed from each side of the scal,
-- it remains balaced.
-- That shows us that the weight of one square is 30 kg.

-- Write the equation represented by this balanced scale, Use /s/ for the wieght of a square.

main :: IO ()
main =
  let
    left :: Int -> Int
    left s = 3 * s

    right :: Int -> Int
    right s = 15 + 2 * s

    checkAndPrint :: Int -> IO ()
    checkAndPrint x
      | x < 100 = do
          if left x == right x
            then print x
            else return ()
          checkAndPrint (x + 1)
      | otherwise = return ()
  in
    checkAndPrint 0

--    mapM_ print [x | x <- [0..99], left x == right x]
