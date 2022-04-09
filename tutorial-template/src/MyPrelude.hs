module MyPrelude
    where

import           Data.List (unzip)
import           Flow      ()

-- | length1
--
--
length1 :: [a] -> Int
length1 []     = 0
length1 (x:xs) = 1 + length1 xs

-- | length2
--
--
length2 :: [a] -> Int
length2 list =
  case list of
    []     -> 0
    (x:xs) ->  1 + length2 xs

-- | length3
--
--
length3 :: [a] -> Int
length3 list = foldl (\acc _ -> acc + 1) 0 list


-- | length4
--
--
length4 :: [a] -> Int
length4 = foldl counter 0
  where
    counter acc _ = acc + 1

-- |
--
--
reverse1 :: [a] -> [a]
reverse1 []     = []
-- reverse1 (x:[])       = (x:[])
-- reverse1 (x:y:[])     = (y:x:[])
-- reverse1 (x:y:z:[])   = (z:y:x:[])
-- reverse1 (x:y:z:w:[]) = (w:z:y:x:[])
reverse1 (x:xs) = reverse1 xs ++ [x]

-- |
--
--
reverse2 :: [a] -> [a]
reverse2 []   = []
-- reverse2 (x:[])       = (x:[])
-- reverse2 (x:y:[])     = (y:x:[])
-- reverse2 (x:y:z:[])   = (z:y:x:[])
-- reverse2 (x:y:z:w:[]) = (w:z:y:x:[])
reverse2 list = last list : reverse2 (init list)

-- |
--
--
transpose1 :: [[a]] -> [[a]]
transpose1 [] = []
transpose1 ([] : xss) = transpose1 xss
transpose1 ((x : xs) : xss) = combine x hds xs tls
  where
    (hds, tls) = unzip [(hd, tl) | hd : tl <- xss]
    combine y h ys t = (y:h) : transpose1 (ys:t)
    {-# NOINLINE combine #-}

-- | head1
--
--
head1 :: [a] -> a
head1 []     = error "head1: empty list"
-- head1 (x:[]) = x
-- head1 (x:y:[])     = x
-- head1 (x:y:z:[])   = x
-- head1 (x:y:z:w:[]) = x
head1 (x:xs) = x

-- | head2
--
--
head2 :: [a] -> a
head2 list =
  case list of
    []     -> error "head2: empty list"
    (x:xs) -> x

-- | tail1
--
--
tail1 :: [a] -> [a]
tail1 []     = error "tail1: empty list"
tail1 (x:[]) = []
-- tail1 (x:y:[])     = y:[]
-- tail1 (x:y:z:[])   = y:z:[]
-- tail1 (x:y:z:w:[]) = y:z:w:[]
tail1 (x:xs) = xs

-- | tail2
--
--
tail2 :: [a] -> [a]
tail2 list =
  case list of
    []     -> error "tail2: empty list"
    (x:[]) -> []
    (x:xs) -> xs

-- | init1
--
--
init1 :: [a] -> [a]
init1 []     = error "init1: empty list"
init1 (x:[]) = []
-- init1 (x:y:[])     = x:[]
-- init1 (x:y:z:[])   = x:y:[]
-- init1 (x:y:z:w:[]) = x:y:z:[]
init1 (x:xs) = x:init1 xs

-- | init2
--
--
init2 :: [a] -> [a]
init2 list =
  case list of
    []     -> error "init2: empty list"
    (x:[]) -> []
    (x:xs) -> x:init2 xs

-- | last1
--
--
last1 :: [a] -> a
last1 []     = error "last1: empty list"
last1 (x:[]) = x
-- last1 (x:y:[])     = y
-- last1 (x:y:z:[])   = z
-- last1 (x:y:z:w:[]) = w
last1 (x:xs) = last1 xs

-- | last2
--
--
last2 :: [a] -> a
last2 list =
  case list of
    []     -> error "last2: empty list"
    (x:[]) -> x
    (x:xs) -> last2 xs

-- | take'
--
--
take' :: Int ->  [a] -> [a]
take' num list =
  if num == 0
  then []
  else
    case list of
      []     -> []
      (x:xs) -> x : take' (num - 1) xs

-- | drop'
--
--
drop' :: Int -> [a] -> [a]
drop' num list =
  if num == 0
  then list
  else
    case list of
      []   -> []
      x:xs -> drop' (num - 1) xs

-- | insert'
--
--
insert' :: Ord a => a -> [a] -> [a]
insert' item list =
  case list of
    [] -> [item]
    x:xs -> if item <= x
            then item : x : xs
            else x : insert' item xs

-- | isort
--
--
isort :: Ord a => [a] -> [a]
isort list =
  case list of
    []   -> []
    x:xs -> insert' x (isort xs)

-- | merge'
--
--
merge' :: Ord a => [a] -> [a] -> [a]
merge' first second =
  case (first, second) of
    ([], second) -> second
    (first, []) -> first
    (x:xs, y:ys) ->
      if x < y
      then x : merge' xs (y:ys)
      else y : merge' (x:xs) ys

-- | msort
--
--
msort :: Ord a => [a] -> [a]
msort ls =
  case ls of
    []  -> []
    [l] -> [l]
    ls  ->
      let left  = take' (length1 ls `div` 2) ls
          right = drop' (length1 ls `div` 2) ls
      in merge' (msort left) (msort right)
