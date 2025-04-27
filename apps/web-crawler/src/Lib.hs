{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import qualified Data.ByteString.Lazy as BL
import           Data.Text            (Text)
import           Data.Text.Encoding   (decodeUtf8)
import qualified Data.Text.IO         as TIO

import           Network.HTTP.Conduit (simpleHttp)

import           Text.HTML.TagSoup


-- 메인 함수
someFunc :: IO ()
someFunc = do
    let url = "http://jsonplaceholder.typicode.com"
    page <- simpleHttp url
    let tags = parseTags page
        links = extractLinks tags
    mapM_ TIO.putStrLn links


-- 헬퍼 함수: 링크 추출
extractLinks :: [Tag BL.ByteString] -> [Text]
extractLinks tags = [decodeUtf8 (BL.toStrict href) | TagOpen "a" attrs <- tags, ("href", href) <- attrs]

