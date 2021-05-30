module Chapter04.Guards
( bmiTell
) where

-- | bmiTell
-- >>> bmiTell 85 1.90
-- "You're supposedly normal. Pffft, I bet you're ugly!"
--
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat    = 30.0

-- | calcBmis
-- >>> calcBmis [(85, 1.90)]
-- [23.545706371191137]
--
calcBmis :: RealFloat a => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2


-- | max'
-- >>> max' 1 2
-- 2
-- >>> max' 2 1
-- 2
-- >>> max' 1 (-1)
-- 1
max' :: (Ord a) => a -> a -> a
max' x y
    | x > y       = x
    | otherwise   = y


-- | myCompare
-- >>> myCompare 1 1
-- EQ
-- >>> myCompare 1 2
-- LT
-- >>> myCompare 2 1
-- GT
--
myCompare :: Ord a => a -> a -> Ordering
myCompare x y
    | x > y     = GT
    | x == y    = EQ
    | otherwise = LT


-- | initials
-- >>> initials "Tom" "Brown"
-- "T. B."
-- 
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname


-- | cylinder
-- >>> cylinder 1.1 2.2
-- 22.8079626650619
--
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea  = pi * r ^ 2
    in sideArea + 2 * topArea


-- | head'
-- >>> head' [1,2,3]
-- 1
--
head' :: [a] -> a
head' []    = error "No head for empty lists!"
head' (x:_) = x

-- | head''
-- >>> head'' [1,2,3]
-- 1
--
head'' :: [a] -> a
head'' xs = case xs of []    -> error "No head for empty list!"
                       (x:_) -> x


-- | describeList
-- >>> describeList [1,2,3]
-- "The list is a longer list."
--
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of []  -> "empty."
                                               [x] -> "a singleton list."
                                               xs  -> "a longer list."


-- | describeList'
-- >>> describeList' [1,2,3]
-- "The list is a longer list."
--
describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what []  = "empty."
          what [x] = "a singleton list."
          what xs  = "a longer list."
