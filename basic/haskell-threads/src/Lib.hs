module Lib
    ( someFunc
    ) where

import           Control.Concurrent          (forkIO)
import           Control.Concurrent.STM.TVar (modifyTVar', newTVar, readTVar)
import           Control.Monad.STM           (atomically, check)
import           Data.Foldable               (for_)
import           System.IO
    ( BufferMode (LineBuffering)
    , hSetBuffering
    , stdout
    )

someFunc :: IO ()
someFunc = do
  hSetBuffering stdout LineBuffering

  tasksCompleted <- atomically $ newTVar @Int 0

  let
    task :: String -> IO ()
    task name =
      -- you can re-write the code below using the `do` notation
      for_ @[] [1 .. 10 :: Int] (\i -> putStrLn $ name <> ": " <> show i) >>= \_ ->
      atomically $ modifyTVar' tasksCompleted (+ 1)

  task "main"
  _ <- forkIO (task "forkA")
  _ <- forkIO (task "forkB")

  -- you can re-write the code below using the `do` notation
  atomically $ readTVar tasksCompleted >>= \x -> check $ x == 3

  putStrLn "done"
