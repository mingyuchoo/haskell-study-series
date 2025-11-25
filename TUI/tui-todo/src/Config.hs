{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
    ( KeyBindings (..)
    , KeyAction (..)
    , loadKeyBindings
    , defaultKeyBindings
    , matchesKey
    ) where

import           Data.Aeson          (FromJSON, parseJSON, withObject, (.:))
import qualified Data.ByteString     as BS
import           Data.List           (find)
import qualified Data.Text           as T
import qualified Data.Yaml           as Yaml
import           GHC.Generics        (Generic)
import qualified Graphics.Vty        as V
import           System.Directory    (doesFileExist)

-- 키 액션 타입
data KeyAction
    = QuitApp
    | AddTodo
    | ToggleComplete
    | DeleteTodo
    | NavigateUp
    | NavigateDown
    | SaveInput
    | CancelInput
    deriving (Eq, Show, Generic)

-- 키바인딩 설정
data KeyBindings = KeyBindings
    { quit            :: [String]
    , add_todo        :: [String]
    , toggle_complete :: [String]
    , delete_todo     :: [String]
    , navigate_up     :: [String]
    , navigate_down   :: [String]
    , save_input      :: [String]
    , cancel_input    :: [String]
    } deriving (Show, Generic)

instance FromJSON KeyBindings where
    parseJSON = withObject "KeyBindings" $ \v -> do
        kb <- v .: "keybindings"
        KeyBindings
            <$> kb .: "quit"
            <*> kb .: "add_todo"
            <*> kb .: "toggle_complete"
            <*> kb .: "delete_todo"
            <*> kb .: "navigate_up"
            <*> kb .: "navigate_down"
            <*> kb .: "save_input"
            <*> kb .: "cancel_input"

-- 기본 키바인딩
defaultKeyBindings :: KeyBindings
defaultKeyBindings = KeyBindings
    { quit = ["q", "Esc"]
    , add_todo = ["a"]
    , toggle_complete = ["Space"]
    , delete_todo = ["d"]
    , navigate_up = ["Up", "k"]
    , navigate_down = ["Down", "j"]
    , save_input = ["Enter"]
    , cancel_input = ["Esc"]
    }

-- 설정 파일에서 키바인딩 로드
loadKeyBindings :: FilePath -> IO KeyBindings
loadKeyBindings path = do
    exists <- doesFileExist path
    if exists
        then do
            content <- BS.readFile path
            case Yaml.decodeEither' content of
                Left err -> do
                    putStrLn $ "키바인딩 설정 파일 로드 실패: " ++ show err
                    putStrLn "기본 키바인딩을 사용합니다."
                    return defaultKeyBindings
                Right kb -> do
                    putStrLn "키바인딩 설정을 로드했습니다."
                    return kb
        else do
            putStrLn $ "설정 파일을 찾을 수 없습니다: " ++ path
            putStrLn "기본 키바인딩을 사용합니다."
            return defaultKeyBindings

-- Vty 키를 문자열로 변환
keyToString :: V.Key -> String
keyToString (V.KChar c) = [c]
keyToString V.KEnter    = "Enter"
keyToString V.KEsc      = "Esc"
keyToString V.KUp       = "Up"
keyToString V.KDown     = "Down"
keyToString V.KLeft     = "Left"
keyToString V.KRight    = "Right"
keyToString V.KBS       = "Backspace"
keyToString (V.KFun n)  = "F" ++ show n
keyToString _           = ""

-- 키가 특정 액션과 매칭되는지 확인
matchesKey :: KeyBindings -> V.Key -> Maybe KeyAction
matchesKey kb key =
    let keyStr = keyToString key
        actions =
            [ (quit kb, QuitApp)
            , (add_todo kb, AddTodo)
            , (toggle_complete kb, ToggleComplete)
            , (delete_todo kb, DeleteTodo)
            , (navigate_up kb, NavigateUp)
            , (navigate_down kb, NavigateDown)
            , (save_input kb, SaveInput)
            , (cancel_input kb, CancelInput)
            ]
    in case find (\(keys, _) -> any (== keyStr) keys) actions of
        Just (_, action) -> Just action
        Nothing          -> Nothing
