{-# LANGUAGE StandaloneKindSignatures #-}

module BinaryTree
    where

import           Data.Kind ()
import           Flow      ()

-- |
--
--
type Tree :: * -> *
data Tree a = Leaf
            | Branch a (Tree a) (Tree a)
            deriving (Show)

-- |
--
--
tree :: Tree Int
tree = Branch 1 (Branch 2 Leaf Leaf) (Branch 3 Leaf Leaf)

-- |
--
--
size :: Tree a -> Int
size Leaf           = 0
size (Branch _ l r) = 1 + size l + size r

-- |
--
--
total :: Tree Int -> Int
total Leaf           = 0
total (Branch x l r) = x + total l + total r

-- |
--
--
maxDepth :: Tree a -> Int
maxDepth Leaf           = 0
maxDepth (Branch _ l r) = 1 + max' (maxDepth l) (maxDepth r)
  where
    max' :: Ord a => a -> a -> a
    max' x y | x > y     = x
             | otherwise = y

-- |
--
--
tList :: Tree a -> [a]
tList Leaf           = []
tList (Branch x l r) = [x] ++ tList l ++ tList r

-- |
--
--
tMap :: (a -> b) -> Tree a -> Tree b
tMap _ Leaf           = Leaf
tMap f (Branch x l r) = Branch (f x) (tMap f l) (tMap f r)

-- |
--
--
lookup' :: (Ord a) => Tree (a,b) -> a -> Maybe b
lookup' Leaf _ = Nothing
lookup' (Branch (k',v') l r) k
  | k == k'  = Just v'
  | otherwise = if k < k' then lookup' l k
                          else lookup' r k

-- |
--
--
insert :: Ord a => Tree (a,b) -> a -> b -> Tree (a,b)
insert Leaf k v = Branch (k,v) Leaf Leaf
insert (Branch (k',v') l r) k v
  | k == k'   = Branch (k,v) l r
  | k < k'    = Branch (k',v') (insert l k v)  r
  | otherwise = Branch (k',v') l (insert r k v)
