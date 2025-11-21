
module Lib
    where

import           Data.Functor ((<&>))
import           Data.IORef   (IORef, modifyIORef, newIORef, readIORef)
import           Data.Kind    (Constraint, Type)

-- |
--
type Counter :: Type -> Type
newtype Counter a = Counter { counter :: IORef a }


-- | make Counter data
--
--
makeCounter :: Int -> IO (Counter Int)
makeCounter i = newIORef i >>= return . Counter

-- | increase Counter data
--
--
incCounter :: Int -> Counter Int -> IO (Counter Int)
incCounter i (Counter counter) = modifyIORef counter (+ i) >> return (Counter counter)

-- | show Counter data
--
--
showCounter :: (Counter Int) -> IO ()
showCounter (Counter counter) = readIORef counter >>= print

-- | apply functions
--
--
someFunc :: IO ()
someFunc = makeCounter 5 >>= incCounter 10 >>= showCounter

