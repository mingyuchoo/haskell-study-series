module Main
    ( main
    ) where

import           Lib
import           System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

-- | Entry point of the application.
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    startApp
