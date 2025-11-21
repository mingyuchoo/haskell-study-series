module Example
    where

import           Control.Lens (element, (&), (.~))

import           Data.List    ((\\))

-- |
--
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

-- |
--
visit :: [Bool]
visit = v & element 1 .~ True
  where
    v = replicate (length graph) False

-- |
--
setVisit :: [Bool] -> Int -> [Bool]
setVisit v i = v & element i .~ True


-- | DFS(Depth-First Search)
--
type BreadcrumbD = ([Int], [Bool])

-- |
--
dfsEval :: [Int]
dfsEval = reverse lst
  where
    lst = fst $ dfs ([1], visit) (graph !! 1)

-- |
--
dfs :: BreadcrumbD -> [Int] -> BreadcrumbD
dfs b [] = b
dfs b@(r, v) (x:xs)
  | v !! x    = dfs b xs
  | otherwise = dfs (dfs (x:r, setVisit v x) (graph !! x)) xs


-- BFS(Breadth-First Search)

type BreadcrumbB = ([Int], [Bool], [Int])

-- |
--
bfsEval :: [Int]
bfsEval = reverse r
  where
    (r, _, _) = bfs ([1], visit, graph !! 1)

-- |
--
bfs :: BreadcrumbB -> BreadcrumbB
bfs b@(_, _, []) = b
bfs (r, v, x:xs) = bfs (x:r, setVisit v x, xs <> (xList \\ xs))
  where
    getToVisit :: [Bool] -> [Int] -> [Int]
    getToVisit v = filter (\x -> not $ v !! x)

    xList :: [Int]
    xList = getToVisit v (graph !! x)
