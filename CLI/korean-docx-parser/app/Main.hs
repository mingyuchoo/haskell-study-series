module Main
  ( main,
  )
where

import Lib
import System.Directory (createDirectoryIfMissing)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let inputFile = "input/애국가.docx"
      outputFile = "output/애국가.md"

  -- output 디렉터리 생성
  createDirectoryIfMissing True "output"

  -- DOCX를 Markdown으로 변환
  putStrLn $ "Converting " ++ inputFile ++ " to " ++ outputFile ++ "..."
  parseDocxToMarkdown inputFile outputFile
  putStrLn "Done!"
