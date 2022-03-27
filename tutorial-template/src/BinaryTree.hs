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
tree1 :: Tree Int
tree1 = Branch 8
         (Branch 3
            (Branch 1 Leaf Leaf)
            (Branch 6
              (Branch 4 Leaf Leaf)
              (Branch 7 Leaf Leaf)))
         (Branch 10
            (Branch 9 Leaf Leaf)
            (Branch 14
              (Branch 13 Leaf Leaf)
              Leaf))

-- |
--
--
size :: Tree a -> Int
size Leaf           = 0
size (Branch _ l r) = 1 + size l + size r

-- |
--
--
total :: (Num a) => Tree a -> a
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
toList :: Tree a -> [a]
toList Leaf           = []
toList (Branch x l r) = toList l ++ [x] ++ toList r

-- |
--
--
map' :: (a -> b) -> Tree a -> Tree b
map' _ Leaf           = Leaf
map' f (Branch x l r) = Branch (f x) (map' f l) (map' f r)

tree2 :: Tree (Int,String)
tree2 = Branch (8,"8")
         (Branch (3,"3")
            (Branch (1,"1") Leaf Leaf)
            (Branch (6,"6")
              (Branch (4,"4") Leaf Leaf)
              (Branch (7,"7") Leaf Leaf)))
         (Branch (10,"10")
            (Branch (9,"9") Leaf Leaf)
            (Branch (14,"14")
              (Branch (13,"13") Leaf Leaf)
              Leaf))
-- |
--
--
lookup' :: (Ord a) => Tree (a,b) -> a -> Maybe b
lookup' Leaf _ = Nothing
lookup' (Branch (k',v') l r) k | k == k'   = Just v'
                               | k < k'    = lookup' l k
                               | k > k'    = lookup' r k
                               | otherwise = Nothing

-- |
--
--
insert :: Ord a => Tree (a,b) -> a -> b -> Tree (a,b)
insert Leaf k v = Branch (k,v) Leaf Leaf
insert (Branch (k',v') l r) k v | k < k'    = Branch (k',v') (insert l k v)  r
                                | k > k'    = Branch (k',v') l (insert r k v)
                                | otherwise = Branch (k,v) l r
