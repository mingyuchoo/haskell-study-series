module Lib
    ( tuiMain
    , runFzf
    ) where

import Brick (defaultMain)
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import System.Environment (getArgs)
import System.IO (hSetBuffering, stdin, stdout, BufferMode(..))

import AppState
import UI
import InputReader
import qualified Cache as C

-- | FZF 메인 함수
tuiMain :: IO ()
tuiMain = runFzf

-- | FZF 실행
runFzf :: IO ()
runFzf = do
    -- 버퍼링 설정
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout NoBuffering
    
    -- 입력 읽기
    items <- readInputLines
    
    if Vec.null items
        then putStrLn "No input provided"
        else do
            -- 캐시 초기화
            cache <- C.newCache
            
            -- 초기 상태
            let initState = initialState items
            
            -- Brick 앱 실행
            _ <- defaultMain (theApp cache) initState
            
            return ()

-- | 버전 정보
versionInfo :: String
versionInfo = "fzh 0.1.0 - Fuzzy Finder in Haskell"
