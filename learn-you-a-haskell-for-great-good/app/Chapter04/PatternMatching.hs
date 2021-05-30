module Chapter04.PatternMatching
( lucky
, sayMe
, factorial
, charName
) where


-- | lucky
-- >>> lucky 7
-- "LUCKY NUMBER SEVEN!"
-- >>> lucky 1
-- "Sorry, you're out of luck, pal!"
--
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"


-- | sayMe
-- >>> sayMe 1
-- "One!"
-- >>> sayMe 2
-- "Two!"
-- >>> sayMe 3
-- "Three!"
-- >>> sayMe 4
-- "Four!"
-- >>> sayMe 5
-- "Five!"
-- >>> sayMe 0
-- "Not between 1 and 5"
--
sayMe :: Integral a => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

-- | factorial
-- >>> factorial 1
-- 1
-- >>> factorial 2
-- 2
-- >>> factorial 3
-- 6
-- >>> factorial 4
-- 24
-- >>> factorial 5
-- 120
-- >>> factorial 50
-- 30414093201713378043612608166064768844377641568960512000000000000
-- 
factorial  :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- | charName
-- >>> charName 'a'
-- "Alpha"
-- >>> charName 'b'
-- "Bravo"
-- >>> charName 'c'
-- "Charlie"
-- >>> charName 'd'
-- "Delta"
--
charName :: Char -> String
charName 'a' = "Alpha"
charName 'b' = "Bravo"
charName 'c' = "Charlie"
charName 'd' = "Delta"

-- | addVectors
-- >>> addVectors (1,2) (2,3)
-- (3,5)
addVectors :: Num a => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


-- | first
-- >>> first (1, 2, 3)
-- 1
-- >>> first ('a', 'b', 'c')
-- 'a'
--
first :: (a, b, c) -> a
first (x, _, _) = x


-- | second
-- >>> second (1, 2, 3)
-- 2
-- >>> second ([1], [2], [3])
-- [2]
--
second :: (a, b, c) -> b
second (_, y, _) = y

-- | third
-- >>> third ((1, "a"), [2], 3.0)
-- 3.0
--
third :: (a, b, c)-> c
third (_, _, z) = z


-- | head'
-- >>> head' [4,5,6]
-- 4
-- >>> head' "Hello"
-- 'H'
--
head' :: [a] -> a
head' [] = error "Can't call head on an empy list, dummy!"
head' (x : _) = x

-- | tell
-- >>> tell []
-- "The list is empty"
--
tell :: (Show a)=> [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long, The first two elements are: " ++ show x ++ " and " ++ show y


-- | length'
-- >>> length' [1, 2, 3, 4, 5]
-- 5
--
length' :: (Num b)=> [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- | sum'
-- >>> sum' [1,2,3,4,5]
-- 15
--
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs


-- | capital
-- >>> capital ""
-- "Empty string, whoops!"
-- >>> capital "Dracula"
-- "The first letter of Dracula is D"
--
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

