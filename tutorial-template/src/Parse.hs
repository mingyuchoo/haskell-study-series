module Parse
    where

import           Data.Char (digitToInt)

-- | number
--
--
number :: [Char] -> Int
number ('-':xs) = numberDigits xs  (0  * (-1))
number ('+':xs) = numberDigits xs  0
number xs       = numberDigits xs  0

-- | numberDigits
--
--
numberDigits :: [Char] -> Int -> Int
numberDigits [] v = v
numberDigits (x:xs) v | x `elem` "0123456789" = numberDigits xs (v*10 + checkZero x)
numberDigits (x:_) _ = error $ "Invalid digit " ++ show x

-- | checkZero
--
--
checkZero :: Char -> Int
checkZero d = if d == '0' then 0 else digitToInt d
