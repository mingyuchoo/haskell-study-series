module Chapter03.TypesAndTypeclasses
    where
--------------------------------------------------------------------------------

-- | removeNonUppercase
-- >>> removeNonUppercase "Hello, Haskell!"
-- "HH"
-- >>> removeNonUppercase ""
-- ""
--
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

-- | addThree
-- >>> addThree 1 1 1
-- 3
-- >>> addThree 1 2 3
-- 6
-- >>> addThree (-1) (-2) 3
-- 0
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z -- function pattern matching

-- | circumference
-- >>> circumference 4.0
-- 25.132741228718345
--
circumference :: Double -> Double
circumference r = 2 * pi * r

--------------------------------------------------------------------------------
