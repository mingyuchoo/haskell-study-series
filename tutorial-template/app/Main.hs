--------------------------------------------------------------------------------
module Main
    where

--------------------------------------------------------------------------------
import           Lib
import           NamingConventions (solution)
import           System.IO
    ( BufferMode (NoBuffering)
    , hSetBuffering
    , stdout
    )

--------------------------------------------------------------------------------
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  someFunc
  solution
