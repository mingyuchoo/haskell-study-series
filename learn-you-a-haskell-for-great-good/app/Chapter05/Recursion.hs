module Chapter05.Recursion
( maximum'
, maximum''
, maximum'''
, replicate'
) where


-- | maximum'
--
maximum' :: (Ord a) => [a] -> a
maximum' []  = error "maximum of empty list"
maximum' [x] = x
maximum' (x : xs)
    | x > maximum' xs = x
    | otherwise       = maximum' xs


-- | maximum''
--
maximum'' :: (Ord a) => [a] -> a
maximum'' []  = error "maximum of empty list"
maximum'' [x] = x
maximum'' (x : xs)
    | x > maxTail = x
    | otherwise   = maxTail
    where maxTail = maximum' xs



-- | maximum'''
--
maximum''' :: (Ord a) => [a] -> a
maximum''' []       = error "maximum of empty list"
maximum''' [x]      = x
maximum''' (x : xs) = max x (maximum' xs)



-- | replicate'
--
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n - 1) x
    
-- | take'
--
take' :: (Num i, Ord i) => i -> [a] => [a]
take' n _
    | n <= 0      = []
take' _ []        = []
take' n (x : xs)  = x : take' (n - 1) xs
