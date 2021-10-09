module Lib
    where

import           Control.Monad
import           Data.Functor  ((<&>))
import           Data.IORef    (IORef, modifyIORef, newIORef, readIORef)


newtype Counter = Counter { counter :: IORef Int }


-- | make Counter data
makeCounter :: Int -> IO Counter
makeCounter i = newIORef i >>= return . Counter

-- | increase Counter data
incCounter :: Int -> Counter -> IO Counter
incCounter i (Counter counter) = modifyIORef counter (+ i) >> return (Counter counter)

-- | show Counter data
showCounter :: Counter -> IO ()
showCounter (Counter counter) = readIORef counter >>= print

-- | apply functions
someFunc :: IO ()
someFunc = makeCounter 5 >>= incCounter 10 >>= showCounter

