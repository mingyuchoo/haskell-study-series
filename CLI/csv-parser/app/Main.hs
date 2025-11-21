module Main
    ( main
    ) where

import           Lib

import           System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  -- 실행
  someFunc filePath
  where
    -- CSV 파일 경로
    filePath :: FilePath
    filePath = "data.csv"
