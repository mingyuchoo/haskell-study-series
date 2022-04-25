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

visited :: [Bool]
visited = do
  let v = take (length graph) [False, False ..]
  v & element 1 .~ True


setVisit :: [Bool] -> Int -> [Bool]
setVisit v i =
  v & element i .~ True


depthIO :: [Bool] -> [Int] -> [Bool]
depthIO v []            = v
depthIO [] _            = []
depthIO v (x:xs) = do
  if v !! x
  then depthIO v xs
  else do
    let visited = setVisit v x
    -- print x
    depthIO (depthIO visited (graph !! x)) xs

-- -----------------------------------------------------------------------------
