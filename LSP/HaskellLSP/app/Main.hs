module Main
    ( main
    ) where

import Flow ((<|), (|>))
import           Lib

import           System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  cliMain
