{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib
    ( AppState (..)
    , Mode (..)
    , Name (..)
    , FocusedField (..)
    , Todo (..)
    , actionEditor
    , subjectEditor
    , indirectObjectEditor
    , directObjectEditor
    , focusedField
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
    , editingIndex
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

-- 모드: 목록 보기 vs 입력 모드 vs 수정 모드
data Mode = ViewMode | InputMode | EditMode DB.TodoId
     deriving (Eq, Show)

-- 리소스 이름
data Name = TodoList 
          | ActionField 
          | SubjectField 
          | IndirectObjectField 
          | DirectObjectField
     deriving (Eq, Ord, Show)

-- 현재 포커스된 필드
data FocusedField = FocusAction | FocusSubject | FocusIndirectObject | FocusDirectObject
     deriving (Eq, Show)

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
data AppState = AppState { _todoList            :: List Name Todo
                         , _actionEditor        :: E.Editor String Name
                         , _subjectEditor       :: E.Editor String Name
                         , _indirectObjectEditor :: E.Editor String Name
                         , _directObjectEditor  :: E.Editor String Name
                         , _focusedField        :: FocusedField
                         , _mode                :: Mode
                         , _dbConn              :: Connection
                         , _keyBindings         :: Config.KeyBindings
                         , _editingIndex        :: Maybe Int
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
      actionText = "할일: " ++ todo ^. todoAction
      subjectText = showField "주체자" (todo ^. todoSubject)
      indirectObjText = showField "대상자" (todo ^. todoIndirectObject)
      directObjText = showField "작업대상" (todo ^. todoDirectObject)
      
      -- 메인 정보 라인
      mainInfo = actionText ++ subjectText ++ indirectObjText ++ directObjText
      
      -- 완료 시각 표시
      completedTimeText = case todo ^. todoCompletedAt of
        Just compTime -> "완료: " ++ compTime ++ " | "
        Nothing -> ""
      
      timestamp =
        padLeft Max <|
          withAttr (attrName "timestamp") <|
            str (completedTimeText ++ "생성: " ++ todo ^. todoCreatedAt)
   in withAttr selectAttr <|
        hBox [checkbox, str mainInfo, timestamp]

drawInput :: AppState -> Widget Name
drawInput s =
  let (label, isEditing) = case s ^. mode of
        InputMode -> (" Add New Todo (Tab: 다음 필드, Enter: 저장, Esc: 취소) ", True)
        EditMode _ -> (" Edit Todo (Tab: 다음 필드, Enter: 저장, Esc: 취소) ", True)
        ViewMode -> (" Input (press 'a' to add, 'e' to edit) ", False)
      
      -- 각 필드의 포커스 상태
      actionFocused = isEditing && s ^. focusedField == FocusAction
      subjectFocused = isEditing && s ^. focusedField == FocusSubject
      indirectObjFocused = isEditing && s ^. focusedField == FocusIndirectObject
      directObjFocused = isEditing && s ^. focusedField == FocusDirectObject
      
      -- 필드 렌더링 헬퍼
      renderField fieldLabel editor isFocused =
        let fieldAttr = if isFocused 
                          then attrName "focusedField"
                          else attrName "normalField"
            labelWidget = withAttr fieldAttr <| str (fieldLabel ++ ": ")
            editorWidget = E.renderEditor (str . unlines) isFocused editor
        in hBox [labelWidget, editorWidget]
      
   in if isEditing
        then borderWithLabel (str label) <|
               padAll 1 <|
                 vBox
                   [ renderField "할일 (필수)" (s ^. actionEditor) actionFocused
                   , str " "
                   , renderField "주체자" (s ^. subjectEditor) subjectFocused
                   , str " "
                   , renderField "대상자" (s ^. indirectObjectEditor) indirectObjFocused
                   , str " "
                   , renderField "작업대상" (s ^. directObjectEditor) directObjFocused
                   ]
        else borderWithLabel (str label) <|
               padAll 1 <|
                 str "(press 'a' to add, 'e' to edit)"

drawHelp :: AppState -> Widget Name
drawHelp s =
  padAll 1 <|
    case s ^. mode of
      InputMode -> str "Enter: Save | Esc: Cancel"
      EditMode _ -> str "Enter: Save | Esc: Cancel"
      ViewMode ->
        let kb = s ^. keyBindings
            quitKeys = head (Config.quit kb)
            addKeys = head (Config.add_todo kb)
            toggleKeys = head (Config.toggle_complete kb)
            deleteKeys = head (Config.delete_todo kb)
            upKeys = head (Config.navigate_up kb)
            downKeys = head (Config.navigate_down kb)
        in vBox
          [ str $ addKeys ++ ": Add | e: Edit | " ++ toggleKeys ++ ": Toggle | " 
                  ++ deleteKeys ++ ": Delete | " ++ upKeys ++ "/" ++ downKeys 
                  ++ ": Navigate | " ++ quitKeys ++ ": Quit"
          ]

-- 이벤트 처리
handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent ev = do
  s <- get
  case s ^. mode of
    ViewMode    -> handleViewMode ev
    InputMode   -> handleInputMode ev
    EditMode _  -> handleEditMode ev

handleViewMode :: BrickEvent Name e -> EventM Name AppState ()
handleViewMode (VtyEvent (V.EvKey key [])) = do
  s <- get
  let kb = s ^. keyBindings
  case Config.matchesKey kb key of
    Just Config.QuitApp -> halt
    Just Config.AddTodo -> do
      modify <| mode .~ InputMode
      modify <| focusedField .~ FocusAction
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
              -- DB에서 업데이트된 todo 다시 로드
              updatedTodos <- liftIO $ App.runAppM (App.AppEnv conn) App.loadTodosFromDB
              case Vec.find (\(id', _, _, _, _, _, _, _, _) -> id' == tid) updatedTodos of
                Just (_, _, newCompleted, _, _, _, _, _, newCompletedAt) ->
                  modify <| todoList %~ listModify (\t -> t { _todoCompleted = newCompleted, _todoCompletedAt = newCompletedAt })
                Nothing -> return ()
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
    _ -> case key of
      V.KChar 'e' -> do
        s' <- get
        case listSelected (s' ^. todoList) of
          Nothing -> return ()
          Just idx -> do
            let todos = s' ^. todoList . listElementsL
            case todos Vec.!? idx of
              Nothing -> return ()
              Just todo -> do
                let tid = todo ^. todoId
                modify <| mode .~ EditMode tid
                modify <| editingIndex .~ Just idx
                modify <| focusedField .~ FocusAction
                modify <| actionEditor .~ E.editor ActionField (Just 1) (todo ^. todoAction)
                modify <| subjectEditor .~ E.editor SubjectField (Just 1) (maybe "" id (todo ^. todoSubject))
                modify <| indirectObjectEditor .~ E.editor IndirectObjectField (Just 1) (maybe "" id (todo ^. todoIndirectObject))
                modify <| directObjectEditor .~ E.editor DirectObjectField (Just 1) (maybe "" id (todo ^. todoDirectObject))
      _ -> return ()
handleViewMode _ = return ()

handleInputMode :: BrickEvent Name e -> EventM Name AppState ()
handleInputMode (VtyEvent (V.EvKey key [])) = do
  s <- get
  let kb = s ^. keyBindings
  case Config.matchesKey kb key of
    Just Config.CancelInput -> do
      modify <| mode .~ ViewMode
      modify <| actionEditor .~ E.editor ActionField (Just 1) ""
      modify <| subjectEditor .~ E.editor SubjectField (Just 1) ""
      modify <| indirectObjectEditor .~ E.editor IndirectObjectField (Just 1) ""
      modify <| directObjectEditor .~ E.editor DirectObjectField (Just 1) ""
    Just Config.SaveInput -> do
      s' <- get
      let action = trim $ unlines $ E.getEditContents (s' ^. actionEditor)
          subject = trim $ unlines $ E.getEditContents (s' ^. subjectEditor)
          indirectObj = trim $ unlines $ E.getEditContents (s' ^. indirectObjectEditor)
          directObj = trim $ unlines $ E.getEditContents (s' ^. directObjectEditor)
          
          subjectMaybe = if null subject then Nothing else Just subject
          indirectObjMaybe = if null indirectObj then Nothing else Just indirectObj
          directObjMaybe = if null directObj then Nothing else Just directObj
      
      if not (null action)
        then do
          let conn = s' ^. dbConn
          (newId, timestamp) <- liftIO $ App.runAppM (App.AppEnv conn) $ do
            tid <- App.saveTodoWithFieldsToDB action subjectMaybe indirectObjMaybe directObjMaybe
            todos <- App.loadTodosFromDB
            let maybeTodo = Vec.find (\(id', _, _, _, _, _, _, _, _) -> id' == tid) todos
            case maybeTodo of
              Just (_, _, _, ts, _, _, _, _, _) -> return (tid, ts)
              Nothing -> return (tid, "")
          
          let newTodo = Todo newId action False timestamp subjectMaybe Nothing indirectObjMaybe directObjMaybe Nothing
              currentList = s' ^. todoList
              newList = listInsert 0 newTodo currentList
          modify <| todoList .~ newList
          modify <| mode .~ ViewMode
          modify <| actionEditor .~ E.editor ActionField (Just 1) ""
          modify <| subjectEditor .~ E.editor SubjectField (Just 1) ""
          modify <| indirectObjectEditor .~ E.editor IndirectObjectField (Just 1) ""
          modify <| directObjectEditor .~ E.editor DirectObjectField (Just 1) ""
        else
          modify <| mode .~ ViewMode
    _ -> case key of
      V.KChar '\t' -> do
        s' <- get
        let nextField = case s' ^. focusedField of
              FocusAction -> FocusSubject
              FocusSubject -> FocusIndirectObject
              FocusIndirectObject -> FocusDirectObject
              FocusDirectObject -> FocusAction
        modify <| focusedField .~ nextField
      _ -> do
        s' <- get
        case s' ^. focusedField of
          FocusAction -> zoom actionEditor <| E.handleEditorEvent (VtyEvent (V.EvKey key []))
          FocusSubject -> zoom subjectEditor <| E.handleEditorEvent (VtyEvent (V.EvKey key []))
          FocusIndirectObject -> zoom indirectObjectEditor <| E.handleEditorEvent (VtyEvent (V.EvKey key []))
          FocusDirectObject -> zoom directObjectEditor <| E.handleEditorEvent (VtyEvent (V.EvKey key []))
handleInputMode ev@(VtyEvent _) = do
  s <- get
  case s ^. focusedField of
    FocusAction -> zoom actionEditor <| E.handleEditorEvent ev
    FocusSubject -> zoom subjectEditor <| E.handleEditorEvent ev
    FocusIndirectObject -> zoom indirectObjectEditor <| E.handleEditorEvent ev
    FocusDirectObject -> zoom directObjectEditor <| E.handleEditorEvent ev
handleInputMode _ = return ()

handleEditMode :: BrickEvent Name e -> EventM Name AppState ()
handleEditMode (VtyEvent (V.EvKey key [])) = do
  s <- get
  let kb = s ^. keyBindings
  case Config.matchesKey kb key of
    Just Config.CancelInput -> do
      modify <| mode .~ ViewMode
      modify <| editingIndex .~ Nothing
      modify <| actionEditor .~ E.editor ActionField (Just 1) ""
      modify <| subjectEditor .~ E.editor SubjectField (Just 1) ""
      modify <| indirectObjectEditor .~ E.editor IndirectObjectField (Just 1) ""
      modify <| directObjectEditor .~ E.editor DirectObjectField (Just 1) ""
    Just Config.SaveInput -> do
      s' <- get
      let action = trim $ unlines $ E.getEditContents (s' ^. actionEditor)
          subject = trim $ unlines $ E.getEditContents (s' ^. subjectEditor)
          indirectObj = trim $ unlines $ E.getEditContents (s' ^. indirectObjectEditor)
          directObj = trim $ unlines $ E.getEditContents (s' ^. directObjectEditor)
          
          subjectMaybe = if null subject then Nothing else Just subject
          indirectObjMaybe = if null indirectObj then Nothing else Just indirectObj
          directObjMaybe = if null directObj then Nothing else Just directObj
      
      case s' ^. mode of
        EditMode tid -> do
          if not (null action)
            then do
              let conn = s' ^. dbConn
              -- DB 업데이트
              case s' ^. editingIndex of
                Nothing -> return ()
                Just idx -> do
                  let todos = s' ^. todoList . listElementsL
                  case todos Vec.!? idx of
                    Nothing -> return ()
                    Just oldTodo -> do
                      liftIO $ App.runAppM (App.AppEnv conn) $ 
                        App.updateTodoInDB tid action subjectMaybe indirectObjMaybe directObjMaybe
                      
                      -- 리스트 업데이트
                      let updatedTodo = oldTodo 
                            { _todoAction = action
                            , _todoSubject = subjectMaybe
                            , _todoIndirectObject = indirectObjMaybe
                            , _todoDirectObject = directObjMaybe
                            }
                      modify <| todoList %~ listModify (const updatedTodo)
              
              modify <| mode .~ ViewMode
              modify <| editingIndex .~ Nothing
              modify <| actionEditor .~ E.editor ActionField (Just 1) ""
              modify <| subjectEditor .~ E.editor SubjectField (Just 1) ""
              modify <| indirectObjectEditor .~ E.editor IndirectObjectField (Just 1) ""
              modify <| directObjectEditor .~ E.editor DirectObjectField (Just 1) ""
            else do
              modify <| mode .~ ViewMode
              modify <| editingIndex .~ Nothing
        _ -> return ()
    _ -> case key of
      V.KChar '\t' -> do
        s' <- get
        let nextField = case s' ^. focusedField of
              FocusAction -> FocusSubject
              FocusSubject -> FocusIndirectObject
              FocusIndirectObject -> FocusDirectObject
              FocusDirectObject -> FocusAction
        modify <| focusedField .~ nextField
      _ -> do
        s' <- get
        case s' ^. focusedField of
          FocusAction -> zoom actionEditor <| E.handleEditorEvent (VtyEvent (V.EvKey key []))
          FocusSubject -> zoom subjectEditor <| E.handleEditorEvent (VtyEvent (V.EvKey key []))
          FocusIndirectObject -> zoom indirectObjectEditor <| E.handleEditorEvent (VtyEvent (V.EvKey key []))
          FocusDirectObject -> zoom directObjectEditor <| E.handleEditorEvent (VtyEvent (V.EvKey key []))
handleEditMode ev@(VtyEvent _) = do
  s <- get
  case s ^. focusedField of
    FocusAction -> zoom actionEditor <| E.handleEditorEvent ev
    FocusSubject -> zoom subjectEditor <| E.handleEditorEvent ev
    FocusIndirectObject -> zoom indirectObjectEditor <| E.handleEditorEvent ev
    FocusDirectObject -> zoom directObjectEditor <| E.handleEditorEvent ev
handleEditMode _ = return ()

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
      (attrName "inputHelp", fg V.cyan `V.withStyle` V.dim),
      (attrName "focusedField", fg V.cyan `V.withStyle` V.bold),
      (attrName "normalField", fg V.white),
      (listSelectedAttr, V.black `on` V.cyan)
    ]

-- 애플리케이션 정의
app :: App AppState e Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = \s locs -> case s ^. mode of
        InputMode -> case s ^. focusedField of
          FocusAction -> showCursorNamed ActionField locs
          FocusSubject -> showCursorNamed SubjectField locs
          FocusIndirectObject -> showCursorNamed IndirectObjectField locs
          FocusDirectObject -> showCursorNamed DirectObjectField locs
        EditMode _ -> case s ^. focusedField of
          FocusAction -> showCursorNamed ActionField locs
          FocusSubject -> showCursorNamed SubjectField locs
          FocusIndirectObject -> showCursorNamed IndirectObjectField locs
          FocusDirectObject -> showCursorNamed DirectObjectField locs
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
            _actionEditor = E.editor ActionField (Just 1) "",
            _subjectEditor = E.editor SubjectField (Just 1) "",
            _indirectObjectEditor = E.editor IndirectObjectField (Just 1) "",
            _directObjectEditor = E.editor DirectObjectField (Just 1) "",
            _focusedField = FocusAction,
            _mode = ViewMode,
            _dbConn = conn,
            _keyBindings = kb,
            _editingIndex = Nothing
          }

  _ <- defaultMain app initialState
  return ()
