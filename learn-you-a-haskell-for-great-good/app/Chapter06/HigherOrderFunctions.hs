module Chapter06.HigherOrderFunctions
( multThree
, compareWithHundred
) where

-- | multThree
--
multThree :: Num a => a -> a -> a -> a
multThree x y z = x * y * z


-- | compareWithHundred
--
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100


-- | divideByTen
--
devideByTen :: Floating a => a -> a
devideByTen = (/10)

