
module Main
    where


import           Lib
import           System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)


-- | main
--
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  someFunc
