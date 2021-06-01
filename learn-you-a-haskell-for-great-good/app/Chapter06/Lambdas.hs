module Chapter06.Lambdas
(
) where

-- | addThree'
-- >>> addThree' 2 5 7
-- 14
--
addThree' :: (Num a) => a -> a -> a -> a
addThree' x y z = x + y + z

-- | addThree''
-- >>> addThree'' 2 5 7
-- 14
--
addThree'' :: (Num a) => a -> a -> a -> a
addThree'' = \x -> \y -> \z -> x + y + z

-- | filp''
-- >>> flip'' zip [1,2,3,4,5] "hello"
-- [('h',1),('e',2),('l',3),('l',4),('o',5)]
--
flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x
