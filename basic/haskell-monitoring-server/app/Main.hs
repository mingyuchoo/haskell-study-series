module Main
    ( main
    ) where

import           Lib
import           System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  someFunc
