{-# LANGUAGE DeriveFoldable #-}

import           Data.Foldable

data Tree a = Leaf
            | Node (Tree a) a (Tree a)
            deriving (Show)

data Inorder a = ILeaf
               | INode (Inorder a) a (Inorder a) -- as before
               deriving (Foldable)

data Preorder a = PrLeaf
                | PrNode a (Preorder a) (Preorder a)
                deriving (Foldable)

data Postorder a = PoLeaf
                 | PoNode (Postorder a) (Postorder a) a
                 deriving Foldable

instance Foldable Tree where
    foldMap f Leaf         = mempty
    foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r

    foldr f acc Leaf         = acc
    foldr f acc (Node l x r) = foldr f (f x (foldr f acc r)) l

-- injections from the earlier Tree type
inorder :: Tree a -> Inorder a
inorder Leaf         = ILeaf
inorder (Node l x r) = INode (inorder l) x (inorder r)

preorder :: Tree a -> Preorder a
preorder Leaf         = PrLeaf
preorder (Node l x r) = PrNode x (preorder l) (preorder r)

postorder :: Tree a -> Postorder a
postorder Leaf         = PoLeaf
postorder (Node l x r) = PoNode (postorder l) (postorder r) x

-- | main
main :: IO ()
main = do
    let myTree = Node (Node Leaf 'a' Leaf) 'b' (Node Leaf 'c' Leaf)
    print myTree
    print $ toList myTree
    print $ toList (inorder   myTree)
    print $ toList (preorder  myTree)
    print $ toList (postorder myTree)

