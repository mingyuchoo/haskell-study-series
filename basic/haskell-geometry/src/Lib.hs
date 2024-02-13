module Lib
    ( someFunc
    ) where

{-

Q: Draw below

*****
****
***
**
*

 -}

someFunc :: IO ()
someFunc = do
  let
    count = 5
  putStr $ solution1 count
  putStr $ solution2 count
  putStr $ solution3 count


solution1 :: Int -> String
solution1 0 = ""
solution1 n = replicate n '*' ++ "\n" ++ solution1 (n-1)


solution2 :: Int -> String
solution2 n =
  case n of 0 -> ""
            _ -> replicate n '*' ++ "\n" ++ solution2 (n-1)


solution3 :: Int -> String
solution3 n
  | n > 0 = replicate n '*' ++ "\n" ++ solution3 (n-1)
  | otherwise = ""
