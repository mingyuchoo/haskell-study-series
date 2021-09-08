-- | factorial: variation 1
--
-- >>> fac1 10
-- 3628800
fac1 :: (Num a, Enum a) => a -> a
fac1 n = product [1..n] -- prouct :: (Foldable t, Num a) => t a -> a


-- | factorial: variation 2
--
-- >>> fac2 10
-- 3628800
fac1 :: (Num a, Enum a) => a -> a
fac2 0 = 1
fac2 n = n * fac2 (n - 1)


-- | main
main :: IO ()
main = do
  print $ fac1 10
  print $ fac2 10
