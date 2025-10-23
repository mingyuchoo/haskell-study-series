module Main
    ( main
    ) where

import           Lib

import           System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
    let inputFile = "input/애국가.docx"
        outputFile = "output/애국가.md"

    -- output 디렉터리 생성
    createDirectoryIfMissing True "output"

    -- DOCX를 Markdown으로 변환
    putStrLn $ "Converting " ++ inputFile ++ " to " ++ outputFile ++ "..."
    parseDocxToMarkdown inputFile outputFile
    putStrLn "Done!"
