module Lib
    ( isPalindrome
    ) where

import           Data.Char (isPunctuation, toLower)

-- |
--
isPalindrome :: String -> Maybe Bool
isPalindrome = isOwnReverseMaybe . rejectEmpty . normalize

-- |
--
rejectEmpty :: String -> Maybe String
rejectEmpty x =
  case x of
    "" -> Nothing
    _  -> Just x

-- |
--
normalize :: String -> String
normalize =
  filter notPunctuation . filter notSpace . allLowerCase

-- |
--
isOwnReverseMaybe :: Maybe String -> Maybe Bool
isOwnReverseMaybe maybeX =
  case maybeX of
    Nothing -> Nothing
    Just x  -> Just (isOwnReverse x)

-- |
--
isOwnReverse :: String -> Bool
isOwnReverse x =
  let
    len = length x
    (l, r)= (0, len - 1)
  in
    all (\i -> x !! i == x !! (r - i)) [l .. r]

-- |
--
nonemptyPal :: String -> Maybe Bool
nonemptyPal x =
  case x of
    "" -> Nothing
    _  -> Just (isOwnReverse x)

-- |
--
allLowerCase :: String -> String
allLowerCase = map toLower

-- |
--
isPalindromeIgnoriingCase :: String -> Bool
isPalindromeIgnoriingCase = isOwnReverse . allLowerCase

-- |
--
isPalindromePhrase :: String -> Bool
isPalindromePhrase = isOwnReverse . filter notSpace

-- |
--
notSpace :: Char -> Bool
notSpace = (/= ' ')

-- |
--
notPunctuation :: Char -> Bool
notPunctuation = not . isPunctuation
