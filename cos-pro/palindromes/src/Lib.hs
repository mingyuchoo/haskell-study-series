module Lib
    where

isPalindrome :: String -> Bool
isPalindrome word =
  word == myReverse word

myReverse :: [Char] -> [Char]
myReverse [] = []
myReverse xs = myReverse (tail xs) ++ head xs : []

someFunc :: IO ()
someFunc = do
  print $ isPalindrome "mom"
