{-# LANGUAGE OverloadedStrings #-}

module LibSpec (spec) where

import Test.Hspec
import Lib
import qualified Data.Text as T
import Text.XML
import System.Directory (doesFileExist, removeFile)

spec :: Spec
spec = do
    describe "parseDocxToMarkdown" $ do
        it "converts DOCX file to Markdown" $ do
            let inputFile = "input/애국가.docx"
                outputFile = "test-output.md"
            
            -- 입력 파일 존재 확인
            inputExists <- doesFileExist inputFile
            inputExists `shouldBe` True
            
            -- 변환 실행
            parseDocxToMarkdown inputFile outputFile
            
            -- 출력 파일 생성 확인
            outputExists <- doesFileExist outputFile
            outputExists `shouldBe` True
            
            -- 정리
            removeFile outputFile
    
    describe "XML parsing functions" $ do
        it "extracts text from simple paragraph" $ do
            let xmlDoc = createSimpleDoc "테스트 텍스트"
                result = extractTextToMarkdown xmlDoc
            result `shouldSatisfy` T.isInfixOf "테스트 텍스트"
        
        it "handles empty paragraphs" $ do
            let xmlDoc = createSimpleDoc ""
                result = extractTextToMarkdown xmlDoc
            T.strip result `shouldBe` ""
    
    describe "getHeadingLevel" $ do
        it "detects Heading1 style" $ do
            getHeadingLevel "Heading1" `shouldBe` Just 1
        
        it "detects heading 2 style" $ do
            getHeadingLevel "heading 2" `shouldBe` Just 2
        
        it "detects Heading3 style" $ do
            getHeadingLevel "Heading3" `shouldBe` Just 3
        
        it "returns Nothing for normal text" $ do
            getHeadingLevel "Normal" `shouldBe` Nothing
        
        it "returns Nothing for empty style" $ do
            getHeadingLevel "" `shouldBe` Nothing

-- 테스트용 간단한 XML 문서 생성
createSimpleDoc :: T.Text -> Document
createSimpleDoc text =
    let wNs = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
        wName' name = Name name (Just wNs) (Just "w")
        
        textNode = NodeElement $ Element (wName' "t") mempty [NodeContent text]
        runNode = NodeElement $ Element (wName' "r") mempty [textNode]
        paraNode = NodeElement $ Element (wName' "p") mempty [runNode]
        bodyNode = NodeElement $ Element (wName' "body") mempty [paraNode]
        docNode = Element (wName' "document") mempty [bodyNode]
    in Document (Prologue [] Nothing []) docNode []
