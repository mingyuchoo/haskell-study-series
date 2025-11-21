{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import Control.Exception (IOException, tryJust)
import Control.Monad (unless)
import Data.ByteString.Lazy qualified as B
import Data.Csv
import Data.Kind (Type)
import Data.Vector qualified as V
import System.Directory (doesFileExist)
import System.IO.Error

-- | CSV 파일의 각 행을 나타내는 데이터 타입 정의 (필요에 따라 조정)
data Person = Person
  { name :: String,
    age :: Int,
    city :: String
  }
  deriving (Show)

-- FromNamedRecord 인스턴스 정의 (헤더를 기반으로 파싱)
instance FromNamedRecord Person where
  parseNamedRecord m =
    Person
      <$> m .: "name"
      <*> m .: "age"
      <*> m .: "city"

-- | 파일이 없으면 파일을 생성하는 함수
createCSVFileIfNotExist :: FilePath -> IO ()
createCSVFileIfNotExist filePath = do
  exists <- doesFileExist filePath
  unless exists $ do
    -- unless 사용
    putStrLn $ "Creating example CSV file: " ++ filePath
    B.writeFile filePath exampleCSVContent
  where
    -- 예시 CSV 파일 내용 (파일이 없는 경우 생성)
    exampleCSVContent :: B.ByteString
    exampleCSVContent = "name,age,city\nAlice,30,New York\nBob,25,London\nCharlie,40,Paris\n"

-- | 파일 읽기 및 파싱 함수
loadAndParseCSV :: FilePath -> IO (Either String [Person])
loadAndParseCSV filePath = do
  result <- tryJust (\e -> if isDoesNotExistError e then Just e else Nothing) (B.readFile filePath) :: IO (Either IOException B.ByteString)
  case result of
    Left err -> return $ Left $ show err
    Right csvData -> case decodeByName csvData of
      Left err -> return $ Left err
      Right (_, records) -> return $ Right (V.toList records)

-- | Main 로직
someFunc :: FilePath -> IO ()
someFunc filePath = do
  createCSVFileIfNotExist filePath

  result <- loadAndParseCSV filePath

  case result of
    Left err -> putStrLn $ "Error decoding CSV file: " ++ err
    Right records -> do
      putStrLn "Parsed Records:"
      mapM_ print records
