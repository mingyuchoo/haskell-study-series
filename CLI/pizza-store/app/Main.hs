module Main where

import           Lib
import           System.IO (BufferMode (NoBuffering), hSetBuffering, stout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  someFunc
