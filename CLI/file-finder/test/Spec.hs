import Test.Hspec
import Lib
import System.Directory hiding (findFiles)
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)

main :: IO ()
main = hspec spec

-- 테스트용 임시 디렉토리 구조 생성
setupTestFiles :: FilePath -> IO ()
setupTestFiles tmpDir = do
  -- 루트 레벨 파일들
  writeFile (tmpDir </> "file1.txt") "content1"
  writeFile (tmpDir </> "file2.hs") "module Test where"
  writeFile (tmpDir </> "file3.md") "# README"
  writeFile (tmpDir </> "large.txt") (replicate 2000 'x')  -- 2000 bytes
  
  -- 서브 디렉토리와 파일들
  createDirectory (tmpDir </> "subdir1")
  writeFile (tmpDir </> "subdir1" </> "nested1.txt") "nested content"
  writeFile (tmpDir </> "subdir1" </> "nested2.hs") "module Nested where"
  
  createDirectory (tmpDir </> "subdir2")
  writeFile (tmpDir </> "subdir2" </> "deep.txt") "deep content"

spec :: Spec
spec = do
    describe "matchesExtension" $ do
        context "when extension filter is Nothing" $ do
            it "should match any file" $ do
                matchesExtension Nothing "test.txt" `shouldBe` True
                matchesExtension Nothing "test.hs" `shouldBe` True
                matchesExtension Nothing "noext" `shouldBe` True
        
        context "when extension filter is specified" $ do
            it "should match files with correct extension" $ do
                matchesExtension (Just ".txt") "file.txt" `shouldBe` True
                matchesExtension (Just ".hs") "Main.hs" `shouldBe` True
            
            it "should not match files with different extension" $ do
                matchesExtension (Just ".txt") "file.hs" `shouldBe` False
                matchesExtension (Just ".md") "file.txt" `shouldBe` False

    describe "listFilesFlat" $ do
        it "should list only files in the directory (non-recursive)" $ do
            withSystemTempDirectory "test" $ \tmpDir -> do
                setupTestFiles tmpDir
                files <- listFilesFlat tmpDir
                length files `shouldBe` 4  -- file1.txt, file2.hs, file3.md, large.txt

    describe "listFilesRecursive" $ do
        it "should list all files recursively" $ do
            withSystemTempDirectory "test" $ \tmpDir -> do
                setupTestFiles tmpDir
                files <- listFilesRecursive tmpDir
                length files `shouldBe` 7  -- 4 root + 2 subdir1 + 1 subdir2

    describe "findFiles with Options" $ do
        context "when searching with extension filter" $ do
            it "should find only .txt files" $ do
                withSystemTempDirectory "test" $ \tmpDir -> do
                    setupTestFiles tmpDir
                    let options = Options tmpDir (Just ".txt") Nothing False False
                    files <- findFiles options
                    length files `shouldBe` 2  -- file1.txt, large.txt
            
            it "should find only .hs files recursively" $ do
                withSystemTempDirectory "test" $ \tmpDir -> do
                    setupTestFiles tmpDir
                    let options = Options tmpDir (Just ".hs") Nothing True False
                    files <- findFiles options
                    length files `shouldBe` 2  -- file2.hs, nested2.hs
        
        context "when searching with size filter" $ do
            it "should find files larger than minimum size" $ do
                withSystemTempDirectory "test" $ \tmpDir -> do
                    setupTestFiles tmpDir
                    let options = Options tmpDir Nothing (Just 1000) False False
                    files <- findFiles options
                    length files `shouldBe` 1  -- only large.txt (2000 bytes)
        
        context "when searching recursively" $ do
            it "should find files in subdirectories" $ do
                withSystemTempDirectory "test" $ \tmpDir -> do
                    setupTestFiles tmpDir
                    let options = Options tmpDir (Just ".txt") Nothing True False
                    files <- findFiles options
                    length files `shouldBe` 4  -- file1.txt, large.txt, nested1.txt, deep.txt
        
        context "when combining multiple filters" $ do
            it "should apply all filters correctly" $ do
                withSystemTempDirectory "test" $ \tmpDir -> do
                    setupTestFiles tmpDir
                    let options = Options tmpDir (Just ".txt") (Just 100) True False
                    files <- findFiles options
                    -- large.txt (2000 bytes) should match
                    length files `shouldSatisfy` (>= 1)
