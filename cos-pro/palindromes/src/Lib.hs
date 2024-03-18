module Lib
    where

import           Data.Char

isPalindrome :: String -> Bool
isPalindrome x =
  x == myReverse x

myReverse :: [Char] -> [Char]
myReverse [] = []
myReverse xs = myReverse (tail xs) ++ head xs : []

maybePalindrome :: String -> Maybe Bool
maybePalindrome "" = Nothing
maybePalindrome x  = Just $ isPalindrome x

verbose :: String -> String
verbose x =
  case maybePalindrome x of
    Nothing    -> "Please enter a word."
    Just False -> "Sorry, this is not a palindrome."
    Just True  -> "Yay, it's a palindrome!"

ignoreCase :: String -> String
ignoreCase = map toLower

makeWord :: String -> String
makeWord = filter isAlphabet
  where
    isAlphabet :: Char  -> Bool
    isAlphabet c | c `elem` ['a' .. 'z'] = True
                 | c `elem` ['A' .. 'Z'] = True
                 | otherwise = False

someFunc :: IO ()
someFunc = do
  putStr "Input a word to check if it is a palindrome: "
  x <- getLine
  print $ maybePalindrome $ makeWord $ ignoreCase x
