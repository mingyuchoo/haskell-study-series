{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib
    ( AppState (..)
    , Mode (..)
    , Name (..)
    , Todo (..)
    , inputEditor
    , keyBindings
    , mode
    , todoCompleted
    , todoCreatedAt
    , todoList
    , todoAction
    , todoId
    , todoSubject
    , todoObject
    , todoIndirectObject
    , todoDirectObject
    , todoCompletedAt
    , trim
    , tuiMain
    ) where

import           Brick                  (App (..), AttrMap,
                                         BrickEvent (VtyEvent), EventM,
                                         Padding (..), Widget, attrMap,
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
import qualified Config
import qualified DB

-- 모드: 목록 보기 vs 입력 모드
data Mode = ViewMode | InputMode
     deriving (Eq, Show)

-- 리소스 이름
data Name = TodoList | InputField
     deriving (Eq, Ord, Show)

-- Todo 항목 데이터 타입 (DB ID 포함)
data Todo = Todo { _todoId             :: DB.TodoId
                 , _todoAction         :: String
                 , _todoCompleted      :: Bool
                 , _todoCreatedAt      :: String
                 , _todoSubject        :: Maybe String
                 , _todoObject         :: Maybe String
                 , _todoIndirectObject :: Maybe String
                 , _todoDirectObject   :: Maybe String
                 , _todoCompletedAt    :: Maybe String
                 }
     deriving (Show)

makeLenses ''Todo

-- 애플리케이션 상태
data AppState = AppState { _todoList     :: List Name Todo
                         , _inputEditor  :: E.Editor String Name
                         , _mode         :: Mode
                         , _dbConn       :: Connection
                         , _keyBindings  :: Config.KeyBindings
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
      vLimit 20 <|
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
      
      -- 필드 표시 헬퍼
      showField _ Nothing = ""
      showField lbl (Just val) = " | " ++ lbl ++ ": " ++ val
      
      -- 각 필드 구성
      actionText = "행위: " ++ todo ^. todoAction
      subjectText = showField "주체자" (todo ^. todoSubject)
      indirectObjText = showField "대상" (todo ^. todoIndirectObject)
      directObjText = showField "실행개체" (todo ^. todoDirectObject)
      
      -- 메인 정보 라인
      mainInfo = actionText ++ subjectText ++ indirectObjText ++ directObjText
      
      timestamp =
        padLeft Max <|
          withAttr (attrName "timestamp") <|
            str ("생성: " ++ todo ^. todoCreatedAt)
   in withAttr selectAttr <|
        hBox [checkbox, str mainInfo, timestamp]

drawInput :: AppState -> Widget Name
drawInput s =
  let label =
        if s ^. mode == InputMode
          then " Add New Todo (Enter to save, Esc to cancel) "
          else " Input (press 'a' to add) "
      helpText = 
        if s ^. mode == InputMode
          then padLeft (Pad 2) <| withAttr (attrName "inputHelp") <| 
                 str "형식: 행위 | 주체자: 값 | 대상: 값 | 실행개체: 값"
          else str ""
   in vBox
        [ borderWithLabel (str label) <|
            padAll 1 <|
              E.renderEditor (str . unlines) (s ^. mode == InputMode) (s ^. inputEditor)
        , helpText
        ]

drawHelp :: AppState -> Widget Name
drawHelp s =
  padAll 1 <|
    if s ^. mode == InputMode
      then str "Enter: Save | Esc: Cancel"
      else
        let kb = s ^. keyBindings
            quitKeys = head (Config.quit kb)
            addKeys = head (Config.add_todo kb)
            toggleKeys = head (Config.toggle_complete kb)
            deleteKeys = head (Config.delete_todo kb)
            upKeys = head (Config.navigate_up kb)
            downKeys = head (Config.navigate_down kb)
        in vBox
          [ str $ addKeys ++ ": Add todo | " ++ toggleKeys ++ ": Toggle complete | " 
                  ++ deleteKeys ++ ": Delete | " ++ upKeys ++ "/" ++ downKeys 
                  ++ ": Navigate | " ++ quitKeys ++ ": Quit"
          ]

-- 이벤트 처리
handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent ev = do
  s <- get
  case s ^. mode of
    ViewMode  -> handleViewMode ev
    InputMode -> handleInputMode ev

handleViewMode :: BrickEvent Name e -> EventM Name AppState ()
handleViewMode (VtyEvent (V.EvKey key [])) = do
  s <- get
  let kb = s ^. keyBindings
  case Config.matchesKey kb key of
    Just Config.QuitApp -> halt
    Just Config.AddTodo -> modify <| mode .~ InputMode
    Just Config.ToggleComplete -> do
      s' <- get
      case listSelected (s' ^. todoList) of
        Nothing -> return ()
        Just idx -> do
          let todos = s' ^. todoList . listElementsL
          case todos Vec.!? idx of
            Nothing -> return ()
            Just todo -> do
              let tid = todo ^. todoId
                  conn = s' ^. dbConn
              liftIO $ App.runAppM (App.AppEnv conn) (App.toggleTodoInDB tid)
              modify <| todoList %~ listModify (todoCompleted %~ not)
    Just Config.DeleteTodo -> do
      s' <- get
      case listSelected (s' ^. todoList) of
        Nothing  -> return ()
        Just idx -> do
          let todos = s' ^. todoList . listElementsL
          case todos Vec.!? idx of
            Nothing -> return ()
            Just todo -> do
              let tid = todo ^. todoId
                  conn = s' ^. dbConn
              liftIO $ App.runAppM (App.AppEnv conn) (App.deleteTodoFromDB tid)
              modify <| todoList %~ listRemove idx
    Just Config.NavigateUp -> zoom todoList <| handleListEvent (V.EvKey V.KUp [])
    Just Config.NavigateDown -> zoom todoList <| handleListEvent (V.EvKey V.KDown [])
    _ -> return ()
handleViewMode _ = return ()

handleInputMode :: BrickEvent Name e -> EventM Name AppState ()
handleInputMode (VtyEvent (V.EvKey key [])) = do
  s <- get
  let kb = s ^. keyBindings
  case Config.matchesKey kb key of
    Just Config.CancelInput -> do
      modify <| mode .~ ViewMode
      modify <| inputEditor .~ E.editor InputField (Just 1) ""
    Just Config.SaveInput -> do
      s' <- get
      let text = unlines <| E.getEditContents (s' ^. inputEditor)
          trimmedText = trim text
      if not (null trimmedText)
        then do
          let conn = s' ^. dbConn
              (action, subject, indirectObj, directObj) = parseTodoInput trimmedText
          
          if null action
            then modify <| mode .~ ViewMode
            else do
              (newId, timestamp) <- liftIO $ App.runAppM (App.AppEnv conn) $ do
                tid <- App.saveTodoWithFieldsToDB action subject indirectObj directObj
                todos <- App.loadTodosFromDB
                let maybeTodo = Vec.find (\(id', _, _, _, _, _, _, _, _) -> id' == tid) todos
                case maybeTodo of
                  Just (_, _, _, ts, _, _, _, _, _) -> return (tid, ts)
                  Nothing -> return (tid, "")
              
              let newTodo = Todo newId action False timestamp subject Nothing indirectObj directObj Nothing
                  currentList = s' ^. todoList
                  newList = listInsert 0 newTodo currentList
              modify <| todoList .~ newList
              modify <| mode .~ ViewMode
              modify <| inputEditor .~ E.editor InputField (Just 1) ""
        else
          modify <| mode .~ ViewMode
    _ -> zoom inputEditor <| E.handleEditorEvent (VtyEvent (V.EvKey key []))
handleInputMode ev@(VtyEvent _) = zoom inputEditor <| E.handleEditorEvent ev
handleInputMode _ = return ()

-- 유틸리티 함수
trim :: String -> String
trim = unwords . words

-- Todo 입력 파싱 함수
-- 형식: "행위 | 주체자: 값 | 대상: 값 | 실행개체: 값"
parseTodoInput :: String -> (String, Maybe String, Maybe String, Maybe String)
parseTodoInput input =
  let parts = map trim $ splitOn '|' input
      (action, fields) = case parts of
        [] -> ("", [])
        (a:fs) -> (a, fs)
      
      findField prefix = 
        case filter (startsWith prefix) fields of
          [] -> Nothing
          (f:_) -> Just $ trim $ drop (length prefix) f
      
      subject = findField "주체자:"
      indirectObj = findField "대상:"
      directObj = findField "실행개체:"
  in (action, subject, indirectObj, directObj)

splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn delim (c:cs)
  | c == delim = "" : rest
  | otherwise = case rest of
      [] -> [[c]]
      (r:rs) -> (c : r) : rs
  where rest = splitOn delim cs

startsWith :: String -> String -> Bool
startsWith prefix s = take (length prefix) s == prefix

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
      (attrName "inputHelp", fg V.cyan `V.withStyle` V.dim),
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
  -- 키바인딩 설정 로드
  kb <- Config.loadKeyBindings "config/keybindings.yaml"
  
  -- 데이터베이스 연결 및 초기화
  conn <- open "todos.db"
  DB.initDB conn
  
  -- 데이터베이스에서 Todo 로드
  todos <- App.runAppM (App.AppEnv conn) App.loadTodosFromDB
  
  let initialTodos = Vec.map (\(tid, text, completed, createdAt, subj, obj, indObj, dirObj, compAt) -> 
                        Todo tid text completed createdAt subj obj indObj dirObj compAt) todos
      initialState =
        AppState
          { _todoList = list TodoList initialTodos 1,
            _inputEditor = E.editor InputField (Just 1) "",
            _mode = ViewMode,
            _dbConn = conn,
            _keyBindings = kb
          }

  _ <- defaultMain app initialState
  return ()
