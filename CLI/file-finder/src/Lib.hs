{-# LANGUAGE RecordWildCards #-}

module Lib
    ( someFunc
    , Options(..)
    , findFiles
    , listFilesFlat
    , listFilesRecursive
    , matchesExtension
    , matchesSize
    ) where

import Options.Applicative
import System.Directory hiding (findFiles)
import System.FilePath
import Control.Monad (filterM, forM_, when)
import Data.List (isSuffixOf)

-- 명령줄 옵션을 담을 데이터 타입
data Options = Options
  { directory   :: FilePath
  , extension   :: Maybe String
  , minSize     :: Maybe Integer
  , recursive   :: Bool
  , verbose     :: Bool
  } deriving (Show)

-- 옵션 파서 정의
optionsParser :: Parser Options
optionsParser = Options
  <$> strOption
      ( long "directory"
     <> short 'd'
     <> metavar "DIR"
     <> value "."
     <> help "검색할 디렉토리 (기본값: 현재 디렉토리)" )
  <*> optional (strOption
      ( long "extension"
     <> short 'e'
     <> metavar "EXT"
     <> help "파일 확장자 필터 (예: .txt, .hs)" ))
  <*> optional (option auto
      ( long "min-size"
     <> short 's'
     <> metavar "BYTES"
     <> help "최소 파일 크기 (바이트)" ))
  <*> switch
      ( long "recursive"
     <> short 'r'
     <> help "하위 디렉토리까지 재귀적으로 검색" )
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "상세 정보 출력" )

-- 파서에 프로그램 정보 추가
opts :: ParserInfo Options
opts = info (optionsParser <**> helper)
  ( fullDesc
  <> progDesc "지정된 조건으로 파일을 검색합니다"
  <> header "file-finder - 강력한 파일 검색 도구" )

-- 파일 검색 로직
findFiles :: Options -> IO [FilePath]
findFiles Options{..} = do
  files <- if recursive
    then listFilesRecursive directory
    else listFilesFlat directory
  
  let filtered = filter (matchesExtension extension) files
  filterM (matchesSize minSize) filtered

-- 디렉토리의 파일 목록 (재귀 없음)
listFilesFlat :: FilePath -> IO [FilePath]
listFilesFlat dir = do
  contents <- listDirectory dir
  let paths = map (dir </>) contents
  filterM doesFileExist paths

-- 디렉토리의 파일 목록 (재귀)
listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive dir = do
  contents <- listDirectory dir
  let paths = map (dir </>) contents
  files <- filterM doesFileExist paths
  dirs <- filterM doesDirectoryExist paths
  subFiles <- concat <$> mapM listFilesRecursive dirs
  return (files ++ subFiles)

-- 확장자 필터
matchesExtension :: Maybe String -> FilePath -> Bool
matchesExtension Nothing _ = True
matchesExtension (Just ext) path = ext `isSuffixOf` path

-- 파일 크기 필터
matchesSize :: Maybe Integer -> FilePath -> IO Bool
matchesSize Nothing _ = return True
matchesSize (Just minBytes) path = do
  size <- getFileSize path
  return (size >= minBytes)

-- 메인 함수
someFunc :: IO ()
someFunc = do
  options <- execParser opts
  
  when (verbose options) $ do
    putStrLn $ "검색 디렉토리: " ++ directory options
    putStrLn $ "옵션: " ++ show options
    putStrLn ""
  
  files <- findFiles options
  
  putStrLn $ "발견된 파일: " ++ show (length files) ++ "개"
  putStrLn ""
  
  forM_ files $ \file -> do
    if verbose options
      then do
        size <- getFileSize file
        putStrLn $ file ++ " (" ++ show size ++ " bytes)"
      else putStrLn file

{- 사용 예시:
   
   # 현재 디렉토리에서 .hs 파일 검색
   ./file-finder -e .hs
   
   # /home/user에서 1KB 이상의 .txt 파일을 재귀적으로 검색
   ./file-finder -d /home/user -e .txt -s 1024 -r -v
   
   # 도움말 보기
   ./file-finder --help
-}