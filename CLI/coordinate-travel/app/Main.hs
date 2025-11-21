module Main
    where

import           Lib

import           System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

-- | main
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  putStr "Input matrix size> "

  size <- getLine

  putStr "command list> "

  commands <- getLine

  print $ foldl (move 5) (1, 1) $ words commands
