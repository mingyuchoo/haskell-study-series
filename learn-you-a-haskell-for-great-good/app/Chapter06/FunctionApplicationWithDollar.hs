module Chapter06.FunctionApplicationWithDollar
(
) where

-- | sum'
--
sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs


-- | oddSquareSum
--
oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- | oddSquareSum'
--
oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

-- | oddSquareSum''
--
oddSquareSum'' :: Integer
oddSquareSum'' =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in sum belowLimit
