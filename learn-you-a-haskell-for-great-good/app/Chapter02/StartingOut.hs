module Chapter02.StartingOut
  ( doubleMe
  , doubleUs
  ) where

-- | doubleMe
--
-- >>> doubleMe 0
-- 0
-- >>> doubleMe 1
-- 2
-- >>> doubleMe 2
-- 4
-- >>> doubleMe 100
-- 200
--
doubleMe :: Int -> Int
doubleMe x = x + x

-- | doubleUs
-- >>> doubleUs 1 1
-- 4
-- >>> doubleUs 1 2
-- 6
-- >>> doubleUs 2 3
-- 10
doubleUs :: Int -> Int -> Int
doubleUs x y = x * 2 + y * 2
