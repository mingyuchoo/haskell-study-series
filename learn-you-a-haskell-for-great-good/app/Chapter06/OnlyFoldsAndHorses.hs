module Chapter06.OnlyFoldsAndHorses
(
) where

-- | sum'
-- 
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- | sum''
-- 
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

