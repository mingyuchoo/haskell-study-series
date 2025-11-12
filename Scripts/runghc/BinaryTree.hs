#!/usr/bin/env runghc

-- Binary Tree Data Structure
data BinaryTree a = Empty
                  | Node a (BinaryTree a) (BinaryTree a)
                  deriving (Show, Eq)

-- Pre-order traversal
preOrder :: BinaryTree a -> [a]
preOrder Empty = []
preOrder (Node value left right) = [value] ++ preOrder left ++ preOrder right

-- In-order traversal
inOrder :: BinaryTree a -> [a]
inOrder Empty = []
inOrder (Node value left right) = inOrder left ++ [value] ++ inOrder right

-- Post-order traversal
postOrder :: BinaryTree a -> [a]
postOrder Empty = []
postOrder (Node value left right) = postOrder left ++ postOrder right ++ [value]

-- Level-order traversal
levelOrder :: BinaryTree a -> [a]
levelOrder tree = concatMap level [0..height tree]
  where
    level n = levelN n tree
    levelN _ Empty = []
    levelN 0 (Node value _ _) = [value]
    levelN n (Node _ left right) = levelN (n-1) left ++ levelN (n-1) right


height :: BinaryTree a -> Int
height Empty = -1
height (Node _ left right) = 1 + max (height left) (height right)


-- Main
main :: IO ()
main = do
  let tree = Node 1
        (Node 2
          (Node 4 Empty Empty)
          (Node 5 Empty Empty))
        (Node 3
          (Node 6 Empty Empty)
          Empty)

  print $ preOrder tree
  print $ inOrder tree
  print $ postOrder tree
  print $ levelOrder tree
