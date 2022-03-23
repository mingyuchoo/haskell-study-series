module Example
    where

import           Control.Monad (guard)
import           Data          (grid)
import           Flow

coords :: [[(Integer, Integer)]]
coords = [ [(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7)]
         , [(1,0),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7)]
         , [(2,0),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7)]
         , [(3,0),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7)]
         , [(4,0),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(4,7)]
         , [(5,0),(5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(5,7)]
         , [(6,0),(6,1),(6,2),(6,3),(6,4),(6,5),(6,6),(6,7)]
         , [(7,0),(7,1),(7,2),(7,3),(7,4),(7,5),(7,6),(7,7)]
         ]

coords2 :: [[(Integer, Integer)]]
coords2 = [0..7] >>= (\row ->
          [ [0..7] >>= (\col ->
            [(row, col)])
          ])

coords3 :: [[(Integer, Integer)]]
coords3 = do
  row <- [0..7]
  return $ do
    col <- [0..7]
    return (row, col)


cols :: [[Integer]]
cols = repeat [0..]

rows :: [[Integer]]
rows = map repeat [0..]

coordsInf :: [[(Integer, Integer)]]
coordsInf = undefined -- zipOverGrid rows cols

repeat8 :: a -> [a]
repeat8 = take 8 . repeat

cols8 :: [[Integer]]
cols8 = repeat8 [0..7]

rows8 :: [[Integer]]
rows8 = map repeat8 [0..7]


grid8 :: [[(Integer, Integer)]]
grid8 = undefined -- zipOverGrid rows8 cols8

foo1 :: Maybe String
foo1 = Just 3 >>= (\x ->
       Just "!" >>= (\y ->
       Just (show x ++ y)))

foo2 :: Maybe String
foo2 = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)

-- variant of outputGrid, for arbitrary Show-able structures
og :: Show a => [a] -> IO ()
og = putStrLn . unlines . map show

-- check if divisible by 2
div2 :: Integral a => a -> Bool
div2 x = mod x 2 == 0


-- List monad notation
mapped :: [Integer]
mapped = do
  i <- [0..]
  return (i * 2)

filtered :: [Integer]
filtered = do
  i <- [0..]
  guard (div2 i)
  return i


mappedAndFiltered ::  [Integer]
mappedAndFiltered = do
  i <- [0..]
  guard (div2 i)
  return (i + 1)

