module Main
    ( main
    ) where

import           Data.String (String)

import           Lib         (isPalindrome)

import           System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  x <- getLine
  print $ verbose x


verbose :: String -> String
verbose x =
  case isPalindrome x of
    Nothing    -> "Please enter a word."
    Just False -> "Sorry, this word is not a palindrome."
    Just True  -> "Congratulations, this word is a palindrome!"
