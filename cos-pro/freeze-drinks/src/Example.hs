module Example
    where

import           Control.Lens (element, (&), (.~))
-- -----------------------------------------------------------------------------

graph :: [[Int]]
graph = [ []
         , [2,3,8]
         , [1,7]
         , [1,4,5]
         , [3,5]
         , [3,4]
         , [7]
         , [2,6,8]
         , [1,7]
         ]

visted :: [Bool]
visted =
  take (length graph) [False, False ..]


setVisit :: [Bool] -> Int -> [Bool]
setVisit v i =
  v & element i .~ True


length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs

length'' :: [a] -> Int
length'' l = foldl (\acc _ -> acc + 1) 0 l


depth :: [Int] -> Int
depth []     = 0 -- base case
depth (x:xs) = depth xs

depthIO :: [Int] -> IO ()
depthIO []     = return ()
depthIO (x:xs) = print x >> depthIO xs

-- -----------------------------------------------------------------------------
