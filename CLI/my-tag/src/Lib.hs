module Lib
    ( someFunc
    ) where

import           Data.Kind          (Type)

import           System.IO          ()

import           Text.Parsec        (anyChar, manyTill, parse, string, try)
import           Text.Parsec.String (Parser)

-- |
--
someFunc :: IO ()
someFunc = do
  input <- readFile "input.txt"
  let output = convertTag input
  writeFile "output.tsx" output
  putStrLn "Completed: output.tsx"

-- |
--
convertTag :: String -> String
convertTag input = case parse parseTitle "" input of
  Left _     -> input
  Right html -> html

-- |
--
parseTitle :: Parser String
parseTitle = do
  _       <- string "<제목>"
  content <- manyTill anyChar (try (string "</제목>"))
  return $ "<MainTitle>" <> content <> "</MainTitle>"
