module BinaryTree
    where

import           Control.Monad (guard)

import           Data.Kind     (Constraint, Type)
import           Data.Maybe    ()

import           Debug.Trace   (trace, traceShow)

-- |
--
--
f :: (Num a, Show a) => a -> a
f x = traceShow ("before: " <> show x <> ", after: " <> show result) result where
  result = x + 1

-- |
--
--
g :: String -> String
g x = trace ("DEBUG: reverse" <> show x) (reverse x)

-- |
--
--
length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs

-- |
--
--
reverse' :: [a] -> [a]
reverse' []   = []
reverse' list = last list : reverse' (init list)

-- |
--
--
head' :: [a] -> a
head' []     = error "head': empty list"
head' (x:xs) = x

-- |
--
--
tail' :: [a] -> [a]
tail' []     = error "tail': empty list"
tail' (x:[]) = []
tail' (x:xs) = xs

-- |
--
--
init' :: [a] -> [a]
init' []     = error "init': empty list"
init' (x:[]) = []
init' (x:xs) = x : init' xs

-- |
--
--
last' :: [a] -> a
last' []     = error "last': empty list"
last' (x:[]) = x
last' (x:xs) = last' xs

-- |
--
--
take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (x:xs) | n > 0 = x : take' (n-1) xs
               | otherwise = []

-- |
--
--
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n (x:xs) | n > 0 = drop' (n-1) xs
               | otherwise = x:xs

-- |
--
--
insert' :: Ord a => a -> [a] -> [a]
insert' v [] = [v]
insert' v list@(x:xs) | v < x = v : list
                      | otherwise = x : insert' v xs

-- |
--
--
isort :: Ord a => [a] -> [a]
isort []          = []
isort [x]         = [x]
isort list@(x:xs) = insert' x (isort xs)

-- |
--
--
merge' :: Ord a => [a] -> [a] -> [a]
merge' [] second = second
merge' first   [] = first
merge' first@(x:xs) second@(y:ys) | x < y = x : merge' xs second
                                  | otherwise = y : merge' first ys

-- |
--
--
msort :: Ord a => [a] -> [a]
msort []          = []
msort [x]         = [x]
msort list@(x:xs) = merge' (msort l) (msort r) where
  half  = div (length list) 2
  l     = take half list
  r     = drop half list

-- |
--
--
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort list@(x:xs) = qsort small <> mid <> qsort large where
  small = [y | y <- xs,   y <  x]
  mid   = [y | y <- list, y == x]
  large = [y | y <- xs,   y >  x]

-- |
--
--
qsort' :: Ord a => [a] -> [a]
qsort' []          = []
qsort' list@(x:xs) = qsort' small <> mid <> qsort' large where
  small = do { y <- xs;   guard (y <  x); return y }
  mid   = do { y <- list; guard (y == x); return y }
  large = do { y <- xs;   guard (y >  x); return y }

-- |
--
--
(-:) :: a -> (a -> b) -> b
(-:) x f = f x

-- |
--
--
type Tree :: Type -> Type
data Tree a = Empty
            | Node a (Tree a) (Tree a)
     deriving (Show)

-- |
--
--
t0 :: Tree Char
t0 =
   Node 'P'
     (Node 'O'
       (Node 'L'
         (Node 'N' Empty Empty)
         (Node 'T' Empty Empty))
       (Node 'Y'
         (Node 'S' Empty Empty)
         (Node 'A' Empty Empty)))
     (Node 'L'
       (Node 'W'
         (Node 'C' Empty Empty)
         (Node 'R' Empty Empty))
       (Node 'A'
         (Node 'A' Empty Empty)
         (Node 'C' Empty Empty)))

-- |
--
--
t1 :: Tree Int
t1 = Node 8
         (Node 3
            (Node 1 Empty Empty)
            (Node 6
              (Node 4 Empty Empty)
              (Node 7 Empty Empty)))
         (Node 10
            (Node 9 Empty Empty)
            (Node 14
              (Node 13 Empty Empty)
              Empty))

-- |
--
--
t2 :: Tree Int
t2 = Node 0
          (Node 1
            (Node 3
              (Node 7 Empty Empty)
              (Node 8 Empty Empty))
            (Node 4
              (Node 9 Empty Empty)
              (Node 10 Empty Empty)))
          (Node 2
            (Node 5
              (Node 11 Empty Empty)
              Empty)
            (Node 6 Empty Empty))

-- |
--
--
type Crumb :: Type -> Type
data Crumb a = LCrumb a (Tree a)
             | RCrumb a (Tree a)
     deriving (Show)

-- |
--
type Breadcrumbs :: Type -> Type
type Breadcrumbs a = [Crumb a]

-- |
--
--
type Zipper :: Type -> Type
type Zipper a = (Tree a, Breadcrumbs a)

-- |
--
--
singleton :: (Ord a, Show a) => a -> Tree a
singleton v = Node v Empty Empty

-- |
--
--
insert :: (Ord a, Show a) => a -> Tree a -> Tree a
insert x Empty = singleton x
insert x (Node v l r) | x == v = Node v l r
                      | x <  v = Node v (insert x l) r
                      | x >  v = Node v l (insert x r)
                      | otherwise = Node v l r

-- |
--
--
elemT :: (Ord a, Show a) => a -> Tree a -> Bool
elemT x Empty = False
elemT x (Node v l r) | x == v = True
                     | x <  v = elemT x l
                     | x >  v = elemT x r
                     | otherwise = False

-- |
--
--
size :: Tree a -> Int
size Empty        = 0
size (Node _ l r) = 1 + size l + size r

-- |
--
--
total :: (Num a) => Tree a -> a
total Empty        = 0
total (Node v l r) = v + total l + total r

-- |
--
--
maxDepth :: Tree a -> Int
maxDepth Empty        = 0
maxDepth (Node _ l r) = 1 + max' (maxDepth l) (maxDepth r) where
  max' :: Ord a => a -> a -> a
  max' x y | x > y     = x
           | otherwise = y

-- | DFS(Depth-First Search): Pre-Order
--
--
preOrder :: (Ord a, Show a) => Tree a -> [a]
preOrder Empty        = []
preOrder (Node v l r) = v : preOrder l <> preOrder r


-- | DFS(Depth-First Search): In-Order
--
--
inOrder :: (Ord a, Show a) => Tree a -> [a]
inOrder Empty        = []
inOrder (Node v l r) = inOrder l <> [v] <> inOrder r

-- | DFS(Depth-First Search): Post-Order
--
--
postOrder :: (Ord a, Show a) => Tree a -> [a]
postOrder Empty        = []
postOrder (Node v l r) = postOrder l <> postOrder r <> [v]

-- | BFS(Breadth-First Search): Level-Order
--
--
levelOrder :: (Ord a, Show a) => Tree a -> [a]
levelOrder t = traversal [t]

traversal :: [Tree a] -> [a]
traversal []                    = []
traversal [Empty]               = []
traversal (Empty : xs)          = traversal xs
traversal (x@(Node v l r) : xs) = v : traversal (xs <> [l, r])

-- |
--
--
map' :: (a -> b) -> Tree a -> Tree b
map' _ Empty        = Empty
map' f (Node v l r) = Node (f v) (map' f l) (map' f r)

-- |
--
--
modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Empty, breadcrumbs)      = (Empty, breadcrumbs)
modify f (Node v l r, breadcrumbs) = (Node (f v) l r, breadcrumbs)


-- |
--
--
attach :: Tree a -> Zipper a -> Zipper a
attach t (_, breadcrumbs) = (t, breadcrumbs)

-- |
--
--
goL :: Zipper a -> Zipper a
goL (Empty, _)                = (Empty, [])
goL (Node v l r, breadcrumbs) = (l, LCrumb v r:breadcrumbs)

-- |
--
--
goR :: Zipper a -> Zipper a
goR (Empty, _)                = (Empty, [])
goR (Node v l r, breadcrumbs) = (r, RCrumb v l:breadcrumbs)

-- |
--
--
goUp :: Zipper a -> Zipper a
goUp (Empty, _)                  = (Empty, [])
goUp (_, [])                     = (Empty, [])
goUp (t, LCrumb v r:breadcrumbs) = (Node v t r, breadcrumbs)
goUp (t, RCrumb v l:breadcrumbs) = (Node v l t, breadcrumbs)

-- |
--
--
topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost zipper  = topMost (goUp zipper)
