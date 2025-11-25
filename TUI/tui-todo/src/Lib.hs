{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib
    ( AppState (..)
    , Mode (..)
    , Name (..)
    , Todo (..)
    , inputEditor
    , mode
    , todoCompleted
    , todoCreatedAt
    , todoList
    , todoText
    , todoId
    , trim
    , tuiMain
    ) where

import           Brick                  (App (..), AttrMap,
                                         BrickEvent (VtyEvent), EventM,
                                         Padding (Max), Widget, attrMap,
                                         attrName, defaultMain, fg, get, hBox,
                                         halt, modify, on, padAll, padLeft,
                                         padTopBottom, showCursorNamed, str,
                                         vBox, vLimit, withAttr, zoom)
import           Brick.Widgets.Border   (borderWithLabel, hBorder)
import           Brick.Widgets.Center   (center, hCenter)
import qualified Brick.Widgets.Edit     as E
import           Brick.Widgets.List     (GenericList (listSelected), List,
                                         handleListEvent, list, listElementsL,
                                         listInsert, listModify, listRemove,
                                         listSelectedAttr, renderList)

import           Control.Monad.IO.Class (liftIO)
import           Database.SQLite.Simple (Connection, open)

import qualified Data.Vector            as Vec

import           Flow                   ((<|))

import qualified Graphics.Vty           as V

import           Lens.Micro             ((%~), (.~), (^.))
import           Lens.Micro.TH          (makeLenses)

import qualified App
import qualified DB

-- 모드: 목록 보기 vs 입력 모드
data Mode = ViewMode | InputMode
     deriving (Eq, Show)

-- 리소스 이름
data Name = TodoList | InputField
     deriving (Eq, Ord, Show)

-- Todo 항목 데이터 타입 (DB ID 포함)
data Todo = Todo { _todoId        :: DB.TodoId
                 , _todoText      :: String
                 , _todoCompleted :: Bool
                 , _todoCreatedAt :: String
                 }
     deriving (Show)

makeLenses ''Todo

-- 애플리케이션 상태
data AppState = AppState { _todoList    :: List Name Todo
                         , _inputEditor :: E.Editor String Name
                         , _mode        :: Mode
                         , _dbConn      :: Connection
                         }

makeLenses ''AppState

-- UI 그리기
drawUI :: AppState -> [Widget Name]
drawUI s = [ui]
  where
    ui =
      vBox
        [ drawHeader,
          hBorder,
          drawTodoList s,
          hBorder,
          drawInput s,
          hBorder,
          drawHelp s
        ]

drawHeader :: Widget Name
drawHeader =
  withAttr (attrName "header") <|
    hCenter <|
      padTopBottom 1 <|
        str "📝 Todo Manager"

drawTodoList :: AppState -> Widget Name
drawTodoList s =
  borderWithLabel (str " Todos ") <|
    padAll 1 <|
      vLimit 15 <|
        if null (s ^. todoList . listElementsL)
          then center <| str "No todos yet. Press 'a' to add one!"
          else renderList drawTodo True (s ^. todoList)

drawTodo :: Bool -> Todo -> Widget Name
drawTodo selected todo =
  let checkbox =
        if todo ^. todoCompleted
          then str "[✓] "
          else str "[ ] "
      todoAttr =
        if todo ^. todoCompleted
          then attrName "completed"
          else attrName "normal"
      selectAttr =
        if selected
          then attrName "selected"
          else todoAttr
      timestamp =
        padLeft Max <|
          withAttr (attrName "timestamp") <|
            str <|
              todo ^. todoCreatedAt
   in withAttr selectAttr <|
        hBox [checkbox, str (todo ^. todoText), timestamp]

drawInput :: AppState -> Widget Name
drawInput s =
  let label =
        if s ^. mode == InputMode
          then " Add New Todo (Enter to save, Esc to cancel) "
          else " Input (press 'a' to add) "
   in borderWithLabel (str label) <|
        padAll 1 <|
          E.renderEditor (str . unlines) (s ^. mode == InputMode) (s ^. inputEditor)

drawHelp :: AppState -> Widget Name
drawHelp s =
  padAll 1 <|
    if s ^. mode == InputMode
      then str "Enter: Save | Esc: Cancel"
      else
        vBox
          [ str "a: Add todo | Space: Toggle complete | d: Delete | ↑↓: Navigate | q: Quit"
          ]

-- 이벤트 처리
handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent ev = do
  s <- get
  case s ^. mode of
    ViewMode  -> handleViewMode ev
    InputMode -> handleInputMode ev

handleViewMode :: BrickEvent Name e -> EventM Name AppState ()
handleViewMode (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleViewMode (VtyEvent (V.EvKey V.KEsc [])) = halt
handleViewMode (VtyEvent (V.EvKey (V.KChar 'a') [])) = do
  modify <| mode .~ InputMode
handleViewMode (VtyEvent (V.EvKey (V.KChar ' ') [])) = do
  s <- get
  case listSelected (s ^. todoList) of
    Nothing -> return ()
    Just idx -> do
      let todos = s ^. todoList . listElementsL
      case todos Vec.!? idx of
        Nothing -> return ()
        Just todo -> do
          let tid = todo ^. todoId
              conn = s ^. dbConn
          liftIO $ App.runAppM (App.AppEnv conn) (App.toggleTodoInDB tid)
          -- UI 상태도 업데이트
          modify <| todoList %~ listModify (todoCompleted %~ not)
handleViewMode (VtyEvent (V.EvKey (V.KChar 'd') [])) = do
  s <- get
  case listSelected (s ^. todoList) of
    Nothing  -> return ()
    Just idx -> do
      let todos = s ^. todoList . listElementsL
      case todos Vec.!? idx of
        Nothing -> return ()
        Just todo -> do
          let tid = todo ^. todoId
              conn = s ^. dbConn
          liftIO $ App.runAppM (App.AppEnv conn) (App.deleteTodoFromDB tid)
          modify <| todoList %~ listRemove idx
handleViewMode (VtyEvent ev) = do
  zoom todoList <| handleListEvent ev
handleViewMode _ = return ()

handleInputMode :: BrickEvent Name e -> EventM Name AppState ()
handleInputMode (VtyEvent (V.EvKey V.KEsc [])) = do
  modify <| mode .~ ViewMode
  modify <| inputEditor .~ E.editor InputField (Just 1) ""
handleInputMode (VtyEvent (V.EvKey V.KEnter [])) = do
  s <- get
  let text = unlines <| E.getEditContents (s ^. inputEditor)
      trimmedText = trim text
  if not (null trimmedText)
    then do
      let conn = s ^. dbConn
      -- 데이터베이스에 저장하고 새 ID 받기
      (newId, timestamp) <- liftIO $ App.runAppM (App.AppEnv conn) $ do
        tid <- App.saveTodoToDB trimmedText
        -- 저장 후 다시 로드하여 타임스탬프 가져오기
        todos <- App.loadTodosFromDB
        let maybeTodo = Vec.find (\(id', _, _, _) -> id' == tid) todos
        case maybeTodo of
          Just (_, _, _, ts) -> return (tid, ts)
          Nothing -> return (tid, "")
      
      let newTodo = Todo newId trimmedText False timestamp
          currentList = s ^. todoList
          newList = listInsert 0 newTodo currentList
      modify <| todoList .~ newList
      modify <| mode .~ ViewMode
      modify <| inputEditor .~ E.editor InputField (Just 1) ""
    else
      modify <| mode .~ ViewMode
handleInputMode ev@(VtyEvent _) = do
  zoom inputEditor <| E.handleEditorEvent ev
handleInputMode _ = return ()

-- 유틸리티 함수
trim :: String -> String
trim = unwords . words

-- 속성 맵
theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (attrName "header", V.white `on` V.blue `V.withStyle` V.bold),
      (attrName "selected", V.black `on` V.cyan),
      (attrName "normal", V.defAttr),
      (attrName "completed", fg V.green `V.withStyle` V.dim),
      (attrName "timestamp", fg V.yellow),
      (listSelectedAttr, V.black `on` V.cyan)
    ]

-- 애플리케이션 정의
app :: App AppState e Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = \s locs -> case s ^. mode of
        InputMode -> showCursorNamed InputField locs
        ViewMode  -> Nothing,
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap = const theMap
    }

tuiMain :: IO ()
tuiMain = do
  -- 데이터베이스 연결 및 초기화
  conn <- open "todos.db"
  DB.initDB conn
  
  -- 데이터베이스에서 Todo 로드
  todos <- App.runAppM (App.AppEnv conn) App.loadTodosFromDB
  
  let initialTodos = Vec.map (\(tid, text, completed, createdAt) -> 
                        Todo tid text completed createdAt) todos
      initialState =
        AppState
          { _todoList = list TodoList initialTodos 1,
            _inputEditor = E.editor InputField (Just 1) "",
            _mode = ViewMode,
            _dbConn = conn
          }

  _ <- defaultMain app initialState
  return ()
