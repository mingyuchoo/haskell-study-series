module Lib
    ( someFunc
    ) where

import           Control.Concurrent.STM.TVar (modifyTVar, modifyTVar', newTVar,
                                              readTVar, writeTVar)
import           Control.Monad.STM           (atomically)

import           Data.Foldable               (for_)
import           Data.Kind                   (Type)

-- |
--
someFunc :: IO ()
someFunc = do
  -- initializes a new variable
  x <- atomically $ newTVar (3 :: Int)

  -- returns the variable's current value
  y <- atomically $ readTVar x
  print y

  -- assigns a new value to a variable
  atomically $ writeTVar x 7
  y <- atomically $ readTVar x
  print y

  -- applies a function to a variable's value
  -- modifyTVar  is non-strict
  -- modifyTVar' is strict
  atomically $ modifyTVar' x (* 2)
  y <- atomically $ readTVar x
  print y

  for_ [1 .. 10] $ \_ -> atomically $ modifyTVar' x (+ 1)
  y <- atomically $ readTVar x
  print y
