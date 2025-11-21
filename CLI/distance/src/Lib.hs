module Lib
    where

-- |
--
getDistance :: ([Char], [Char]) -> Int -> Int
getDistance ([], []) z = z
getDistance ((x : xs), (y : ys)) z
  | x == y    = getDistance (xs, ys) z
  | otherwise = getDistance (xs, ys) (z + 1)


-- |
--
align :: [Char] -> [Char] -> ([Char], [Char])
align x y
  | size >  0 = (x,                (addZero y size))
  | size == 0 = (x,                y)
  | otherwise = ((addZero x size), y)
  where size  = (length x) - (length y)

-- |
--
addZero :: [Char] -> Int -> [Char]
addZero [] _  =  []
addZero x y | y == 0    = x
            | y == 1    = '0' : x
            | otherwise = '0' : (addZero x (y-1))

-- |
--
someFunc :: IO ()
someFunc = putStrLn "someFunc"
