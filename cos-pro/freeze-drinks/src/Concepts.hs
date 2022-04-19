module Concepts
    where

import           Control.Lens (element, (&), (.~))
import           Data.Graph


-- -----------------------------------------------------------------------------
graph :: Graph
(graph, _, _) = graphFromEdges [ (0,0,[1,2])
                               , (1,1,[0,6])
                               , (2,2,[0,3,4])
                               , (3,3,[2,4])
                               , (4,4,[2,3])
                               , (5,5,[6])
                               , (6,6,[1,5,7])
                               , (7,7,[0,6])
                               ]

graph' :: Graph
graph' = buildG (0,7) [ (0,2)
                      , (0,1)
                      , (1,6)
                      , (1,0)
                      , (2,4)
                      , (2,3)
                      , (2,0)
                      , (3,4)
                      , (3,2)
                      , (4,3)
                      , (4,2)
                      , (5,6)
                      , (6,7)
                      , (6,5)
                      , (6,1)
                      , (7,6)
                      , (7,0)
                      ]

-- -----------------------------------------------------------------------------

graph1 :: [[Int]]
graph1 = [ []
         , [2,3,8]
         , [1,7]
         , [1,4,5]
         , [3,5]
         , [3,4]
         , [7]
         , [2,6,8]
         , [1,7]
         ]

visited1 :: [Bool]
visited1 =
  take (length graph1) [False, False ..]


dfs1 :: [[Int]]  -> Int -> [Bool] -> Int
dfs1 g i v =
   undefined


setVisit1 :: [Bool] -> Int -> [Bool]
setVisit1 v i =
  v & element i .~ True

-- -----------------------------------------------------------------------------

newtype Stack a =
  Stack [a] deriving (Show)

emptyS :: Stack a
emptyS = Stack []

-- |
--
-- >>> emptyS `push` 1 `push` 2 `push` 3
-- Stack [3,2,1]
--
push :: Stack a -> a -> Stack a
push (Stack xs) x = Stack (x:xs)

-- |
--
-- >>> pop $ Stack[3,2,1]
-- (Just 3,Stack [2,1])
--
pop :: Stack a -> (Maybe a, Stack a)
pop (Stack [])     = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)

-- -----------------------------------------------------------------------------

newtype Queue a =
  Queue [a] deriving (Show)


emptyQ :: Queue a
emptyQ = Queue []


-- |
--
-- >>> emptyQ `enque` 1 `enque` 2 `enque` 3
-- Stack [1,2,3]
--
enque :: Queue a -> a -> Queue a
enque (Queue xs) x = Queue (xs <> [x])

-- |
--
-- >>> deque $ Queue[1,2,3]
-- (Just 1,Queue [2,3])
--
deque :: Queue a -> (Maybe a, Queue a)
deque (Queue [])     = (Nothing, Queue [])
deque (Queue (x:xs)) = (Just x, Queue xs)

-- -----------------------------------------------------------------------------
