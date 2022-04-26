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

visit :: [Bool]
visit = do
  let v = take (length graph) [False, False ..]
  v & element 1 .~ True


setVisit :: [Bool] -> Int -> [Bool]
setVisit v i =
  v & element i .~ True


type Breadcrumb = ([Int], [Bool])

driver :: Breadcrumb
driver = dfs ([1], visit) (graph !! 1)

dfs :: Breadcrumb -> [Int] ->  Breadcrumb
dfs f [] = f
dfs f@(a, v) (x:xs)
  | v !! x    = dfs f xs
  | otherwise = dfs (dfs (x:a, visited) (graph !! x)) xs
                  where
                    visited = setVisit v x

-- -----------------------------------------------------------------------------
