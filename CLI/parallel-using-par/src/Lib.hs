module Lib
    where

import           Control.Parallel

-- |
--
-- fac
fac :: (Eq p, Num p) => p -> p
fac 0 = 1
fac n = n * fac (n-1)

-- |
--
-- ack
ack :: (Num a, Num t, Eq a, Eq t) => a -> t -> t
ack 0 n = n+1
ack m 0 = ack (m-1) 1
ack m n = ack (m-1) (ack m (n-1))

-- |
--
-- fib
fib :: (Eq a, Num a, Num p) => a -> p
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- |
--
someFunc :: IO ()
someFunc = a `par` b `par` c `pseq` print (a + b + c)
    where
        a = ack 3 10
        b = fac 42
        c = fib 34
