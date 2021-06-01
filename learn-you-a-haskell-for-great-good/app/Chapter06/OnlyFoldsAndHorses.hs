module Chapter06.OnlyFoldsAndHorses
(
) where

-- | sum'
-- >>> sum' [1,2,3,4]
-- 10
--
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- | sum''
-- >>> sum'' [1,2,3,4]
-- 10
--
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0


-- | elem'
--
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys


-- | map''
--
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

