module Example
    where

import           Control.Lens           (element, (&), (.~))
import           Data.List              ((\\))
import           Debug.Trace
import           System.Posix.Internals (lstat)

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

visit :: [Bool]
visit =
  let
    v = take (length graph) [False, False ..]
  in
    v & element 1 .~ True


setVisit :: [Bool] -> Int -> [Bool]
setVisit v i =
  v & element i .~ True

-- -----------------------------------------------------------------------------
-- DFS(Depth-First Search)

type BreadcrumbD = ([Int], [Bool])

dfsEval :: [Int]
dfsEval =
  let
    lst = fst $ dfs ([1], visit) (graph !! 1)
  in
    reverse lst

dfs :: BreadcrumbD -> [Int] ->  BreadcrumbD
dfs b [] = b
dfs b@(r, v) (x:xs)
  | v !! x    = dfs b xs
  | otherwise = dfs (dfs (x:r, visited) (graph !! x)) xs
                  where
                    visited = setVisit v x

-- -----------------------------------------------------------------------------
-- BFS(Breadth-First Search)

type BreadcrumbB = ([Int], [Bool], [Int])

bfsEval :: [Int]
bfsEval =
  let
    (r, v, l) = bfs ([1], visit, graph !! 1)
  in
    reverse r


bfs :: BreadcrumbB -> BreadcrumbB
bfs b@(r, v, []) = b
bfs b@(r, v, x:xs) =
  let
    xList = getToVisit v (graph !! x)
  in
    bfs (x:r, setVisit v x, xs ++ (xList \\ xs))


getToVisit :: [Bool] -> [Int] -> [Int]
getToVisit v = filter (\x -> not $ v !! x)

-- -----------------------------------------------------------------------------
