module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
  putStrLn $ solution ""                        --
  putStrLn $ solution "a"                       -- a1
  putStrLn $ solution "aaaaabbbccccccddddddddd" -- a5b3c6d9


solution :: [Char] -> [Char]
solution [] = ""
solution (x:xs) = encode x xs 1
  where
    encode :: Char -> String -> Int -> String
    encode ch [] count = [ch] ++ show count
    encode ch (y:ys) count
      | ch == y = encode ch ys (count + 1)
      | otherwise = [ch] ++ show count ++ encode y ys 1
