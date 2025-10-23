{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( parseDocxToMarkdown
    ) where

import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.XML
import Text.XML.Cursor

-- DOCX 파일을 파싱하여 Markdown으로 변환
parseDocxToMarkdown :: FilePath -> FilePath -> IO ()
parseDocxToMarkdown inputPath outputPath = do
    -- DOCX 파일 읽기 (ZIP 아카이브)
    archive <- toArchive <$> BL.readFile inputPath
    
    -- document.xml 추출
    let docEntry = findEntryByPath "word/document.xml" archive
    case docEntry of
        Nothing -> putStrLn "Error: document.xml not found in DOCX file"
        Just entry -> do
            let xmlContent = fromEntry entry
            case parseLBS def xmlContent of
                Left err -> putStrLn $ "XML parsing error: " ++ show err
                Right doc -> do
                    let markdown = extractTextToMarkdown doc
                    TIO.writeFile outputPath markdown
                    putStrLn $ "Successfully converted: " ++ outputPath

-- XML 문서에서 텍스트와 스타일 정보를 추출하여 Markdown으로 변환
extractTextToMarkdown :: Document -> Text
extractTextToMarkdown doc =
    let cursor = fromDocument doc
        paragraphs = cursor $// element (wName "p")
        markdownLines = map processParagraph paragraphs
    in T.unlines markdownLines

-- 단락 처리
processParagraph :: Cursor -> Text
processParagraph pCursor =
    let runs = pCursor $// element (wName "r")
        texts = map processRun runs
        paraText = T.concat texts
        -- 단락 스타일 확인 (제목 등)
        pStyles = pCursor $/ element (wName "pPr") &/ element (wName "pStyle") &| attribute "val"
        headingLevel = getHeadingLevel (T.concat $ concat pStyles)
    in if T.null paraText
       then ""
       else case headingLevel of
           Just level -> T.replicate level "#" <> " " <> paraText
           Nothing -> paraText

-- Run (텍스트 조각) 처리
processRun :: Cursor -> Text
processRun rCursor =
    let textNodes = rCursor $/ element (wName "t") &/ content
        text = T.concat textNodes
        -- 텍스트 스타일 확인
        isBold = not $ null $ rCursor $/ element (wName "rPr") &/ element (wName "b")
        isItalic = not $ null $ rCursor $/ element (wName "rPr") &/ element (wName "i")
        isUnderline = not $ null $ rCursor $/ element (wName "rPr") &/ element (wName "u")
    in applyStyles isBold isItalic isUnderline text

-- 스타일 적용
applyStyles :: Bool -> Bool -> Bool -> Text -> Text
applyStyles bold italic underline text
    | T.null text = ""
    | bold && italic = "***" <> text <> "***"
    | bold = "**" <> text <> "**"
    | italic = "*" <> text <> "*"
    | underline = "_" <> text <> "_"
    | otherwise = text

-- 제목 레벨 추출
getHeadingLevel :: Text -> Maybe Int
getHeadingLevel style
    | T.null style = Nothing
    | "Heading1" `T.isInfixOf` style || "heading 1" `T.isInfixOf` style = Just 1
    | "Heading2" `T.isInfixOf` style || "heading 2" `T.isInfixOf` style = Just 2
    | "Heading3" `T.isInfixOf` style || "heading 3" `T.isInfixOf` style = Just 3
    | "Heading4" `T.isInfixOf` style || "heading 4" `T.isInfixOf` style = Just 4
    | otherwise = Nothing

-- Word XML 네임스페이스 헬퍼
wName :: Text -> Name
wName localName = Name localName (Just "http://schemas.openxmlformats.org/wordprocessingml/2006/main") (Just "w")
