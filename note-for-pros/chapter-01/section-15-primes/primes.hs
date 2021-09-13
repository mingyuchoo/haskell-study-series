{------------------------------------------------------------------------------
 - `cabal install -lib data-ordlist`
 ------------------------------------------------------------------------------}

import           Data.List         (nubBy, (\\))
import qualified Data.List.Ordered

-- | below 100
ps100 :: [Integer]
ps100 = ( ( ( [2..100] \\ [4,6..100]
            ) \\ [6,9..100]
          ) \\ [10,15..100]
        ) \\ [14,21..100]


-- | ps1:  variation 1 - traditional
ps1 :: [Integer]
ps1 =
    sieve [2..]
  where
    sieve (x:sx) = [x] ++ sieve [y | y <- sx, rem y x > 0]

-- | ps2:  variation 2 - optimal trial division
ps2 :: [Integer]
ps2 = 2 : [n | n <- [3..], all ((> 0).rem n) $ takeWhile ((<= n).(^2)) ps2]

-- | ps3:  variation 3 - traditional
ps3 :: [Integer]
ps3 = [n | n <- [2..], [] == [i | i <- [2..n-1], j <- [0,i..n], j==n]]

-- | ps4:  variation 4 - the shortest code
ps4 :: [Integer]
ps4 = nubBy (((>1).).gcd) [2..] -- i.e., nubBy (\a b -> gcd a b > 1) [2..]

-- | main
main :: IO ()
main = do
  print   ps100
  print $ take 10 ps1
  print $ take 10 ps2
  print $ take 10 ps3
  print $ take 10 ps4
