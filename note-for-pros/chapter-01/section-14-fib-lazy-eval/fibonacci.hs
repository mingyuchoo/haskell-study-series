-- | fibs: variation 1
fibs1 :: [Int]
fibs1 = 0 : 1 : zipWith (+) fibs1 (tail fibs1)

-- | fibs: variation 2
fibs2 :: [Int]
fibs2 = 0 : scanl (+) 1 fibs2

-- | fibn: variation 1
fibn1 :: Int -> Int
fibn1 n =
    fibs !! n
  where
    fibs = 0 : 1 : map f [2..]
    f n  = fibs !! (n-1) + fibs !! (n-2)

-- | fibn: variation 2
fibn2 :: Int -> Int
fibn2 n = fibs1 !! n -- (!!) being the list subscript operator

-- | fibn: variation 3 in point-free style of variation 2
fibn3 :: Int -> Int
fibn3 = (fibs2 !!)

-- | main
main :: IO ()
main = do
  print $ take 10 fibs1
  print $ take 10 fibs2
  print $ fibn1 10
  print $ fibn2 10
  print $ fibn3 10
