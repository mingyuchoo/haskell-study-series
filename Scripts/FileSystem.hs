#!/usr/bin/env runghc


import System.Directory
import System.FilePath
import Control.Monad (filterM, when)
import qualified Data.ByteString as BS
import Data.List (isPrefixOf)

main :: IO ()
main = do
    putStrLn "=== Haskell 파일/디렉터리 조작 예제 ===\n"
    
    -- 1. 디렉터리 생성
    putStrLn "1. 디렉터리 생성"
    createDirectoryIfMissing True "test_dir/sub_dir"
    putStrLn "   test_dir/sub_dir 생성 완료\n"
    
    -- 2. 현재 디렉터리 확인
    putStrLn "2. 현재 디렉터리"
    currentDir <- getCurrentDirectory
    putStrLn $ "   " ++ currentDir ++ "\n"
    
    -- 3. 파일 생성 및 쓰기
    putStrLn "3. 파일 쓰기"
    writeFile "test_dir/sample.txt" "Hello, Haskell!\n안녕하세요!"
    putStrLn "   test_dir/sample.txt 생성 완료\n"
    
    -- 4. 파일 읽기
    putStrLn "4. 파일 읽기"
    content <- readFile "test_dir/sample.txt"
    putStrLn $ "   내용: " ++ content ++ "\n"
    
    -- 5. 파일에 추가 쓰기
    putStrLn "5. 파일에 추가"
    appendFile "test_dir/sample.txt" "\n추가된 내용"
    putStrLn "   내용 추가 완료\n"
    
    -- 6. 파일 존재 확인
    putStrLn "6. 파일 존재 확인"
    exists <- doesFileExist "test_dir/sample.txt"
    putStrLn $ "   sample.txt 존재: " ++ show exists ++ "\n"
    
    -- 7. 디렉터리 내용 나열
    putStrLn "7. 디렉터리 내용"
    files <- listDirectory "test_dir"
    putStrLn "   test_dir 내용:"
    mapM_ (\f -> putStrLn $ "   - " ++ f) files
    putStrLn ""
    
    -- 8. 파일 복사
    putStrLn "8. 파일 복사"
    copyFile "test_dir/sample.txt" "test_dir/sample_copy.txt"
    putStrLn "   sample_copy.txt 복사 완료\n"
    
    -- 9. 파일 이름 변경/이동
    putStrLn "9. 파일 이름 변경"
    renameFile "test_dir/sample_copy.txt" "test_dir/renamed.txt"
    putStrLn "   renamed.txt로 이름 변경 완료\n"
    
    -- 10. 파일 크기 확인
    putStrLn "10. 파일 크기"
    size <- getFileSize "test_dir/sample.txt"
    putStrLn $ "   sample.txt 크기: " ++ show size ++ " bytes\n"
    
    -- 11. 재귀적으로 디렉터리 탐색
    putStrLn "11. 재귀적 디렉터리 탐색"
    allFiles <- listDirectoryRecursive "test_dir"
    putStrLn "   모든 파일/디렉터리:"
    mapM_ (\f -> putStrLn $ "   - " ++ f) allFiles
    putStrLn ""
    
    -- 12. 파일 삭제
    putStrLn "12. 파일 삭제"
    removeFile "test_dir/renamed.txt"
    putStrLn "   renamed.txt 삭제 완료\n"
    
    -- 13. 디렉터리 삭제 (재귀적)
    putStrLn "13. 디렉터리 삭제"
    removeDirectoryRecursive "test_dir"
    putStrLn "   test_dir 삭제 완료\n"
    
    putStrLn "=== 모든 작업 완료 ==="

-- 재귀적으로 디렉터리 내용을 나열하는 함수
listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive dir = do
    entries <- listDirectory dir
    let fullPaths = map (dir </>) entries
    files <- filterM doesFileExist fullPaths
    dirs <- filterM doesDirectoryExist fullPaths
    subFiles <- concat <$> mapM listDirectoryRecursive dirs
    return $ files ++ dirs ++ subFiles


-- === 추가 유용한 함수들 ===

-- 파일 확장자로 필터링
filterByExtension :: String -> [FilePath] -> [FilePath]
filterByExtension ext = filter (\f -> takeExtension f == ext)

-- 특정 패턴의 파일 찾기
findFiles :: FilePath -> String -> IO [FilePath]
findFiles dir pattern = do
    allFiles <- listDirectoryRecursive dir
    return $ filter (pattern `isPrefixOf`) (map takeFileName allFiles)

-- 안전한 파일 쓰기 (기존 파일 백업)
safeWriteFile :: FilePath -> String -> IO ()
safeWriteFile path content = do
    exists <- doesFileExist path
    when exists $ copyFile path (path ++ ".backup")
    writeFile path content

-- 디렉터리가 비어있는지 확인
isEmptyDirectory :: FilePath -> IO Bool
isEmptyDirectory dir = do
    contents <- listDirectory dir
    return $ null contents
