{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib
    ( AppState (..)
    , FocusedField (..)
    , Mode (..)
    , Name (..)
    , Todo (..)
    , actionEditor
    , directObjectEditor
    , editingIndex
    , focusedField
    , indirectObjectEditor
    , keyBindings
    , mode
    , subjectEditor
    , todoAction
    , todoCompleted
    , todoCompletedAt
    , todoCreatedAt
    , todoDirectObject
    , todoId
    , todoIndirectObject
    , todoList
    , todoObject
    , todoSubject
    , trim
    , tuiMain
    ) where

import qualified App

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

import qualified Config

import           Control.Monad          (void, when)
import           Control.Monad.IO.Class (liftIO)

import qualified DB

import           Data.List              (find)
import           Data.Maybe             (fromMaybe, listToMaybe)
import qualified Data.Vector            as Vec

import           Database.SQLite.Simple (Connection, open)

import           Flow                   ((<|))

import qualified Graphics.Vty           as V

import           Lens.Micro             ((%~), (.~), (^.))
import           Lens.Micro.TH          (makeLenses)

-- Application modes
data Mode = ViewMode
          | InputMode
          | EditMode DB.TodoId
     deriving (Eq, Show)

-- Widget resource names
data Name = TodoList | ActionField | SubjectField | IndirectObjectField | DirectObjectField
     deriving (Eq, Ord, Show)

-- Field focus tracking
data FocusedField = FocusAction | FocusSubject | FocusIndirectObject | FocusDirectObject
     deriving (Eq, Show)

-- UI representation of a Todo item
data Todo = Todo { _todoId             :: !DB.TodoId
                 , _todoAction         :: !String
                 , _todoCompleted      :: !Bool
                 , _todoCreatedAt      :: !String
                 , _todoSubject        :: !(Maybe String)
                 , _todoObject         :: !(Maybe String)
                 , _todoIndirectObject :: !(Maybe String)
                 , _todoDirectObject   :: !(Maybe String)
                 , _todoCompletedAt    :: !(Maybe String)
                 }
     deriving (Eq, Show)

makeLenses ''Todo

-- Application state
data AppState = AppState { _todoList             :: !(List Name Todo)
                         , _actionEditor         :: !(E.Editor String Name)
                         , _subjectEditor        :: !(E.Editor String Name)
                         , _indirectObjectEditor :: !(E.Editor String Name)
                         , _directObjectEditor   :: !(E.Editor String Name)
                         , _focusedField         :: !FocusedField
                         , _mode                 :: !Mode
                         , _dbConn               :: !Connection
                         , _keyBindings          :: !Config.KeyBindings
                         , _editingIndex         :: !(Maybe Int)
                         }

makeLenses ''AppState

-- Convert DB TodoRow to UI Todo
fromTodoRow :: DB.TodoRow -> Todo
fromTodoRow row = Todo
    { _todoId             = DB.todoId row
    , _todoAction         = DB.todoAction row
    , _todoCompleted      = DB.todoCompleted row
    , _todoCreatedAt      = DB.todoCreatedAt row
    , _todoSubject        = DB.todoSubject row
    , _todoObject         = DB.todoObject row
    , _todoIndirectObject = DB.todoIndirectObject row
    , _todoDirectObject   = DB.todoDirectObject row
    , _todoCompletedAt    = DB.todoCompletedAt row
    }

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
          drawDetailView s,
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
drawTodo selected todo = withAttr selectAttr <| hBox [checkbox, str mainInfo, timestamp]
  where
    checkbox = str <| if todo ^. todoCompleted then "[✓] " else "[ ] "

    todoAttr = attrName <| if todo ^. todoCompleted then "completed" else "normal"
    selectAttr = if selected then attrName "selected" else todoAttr

    showField _ Nothing    = ""
    showField lbl (Just v) = " | " ++ lbl ++ ": " ++ v

    mainInfo = concat
        [ "할일: " ++ todo ^. todoAction
        , showField "주체자" (todo ^. todoSubject)
        , showField "대상자" (todo ^. todoIndirectObject)
        , showField "작업대상" (todo ^. todoDirectObject)
        ]

    completedTimeText = maybe "" (\t -> "완료: " ++ t ++ " | ") (todo ^. todoCompletedAt)

    timestamp = padLeft Max <| withAttr (attrName "timestamp") $
        str (completedTimeText ++ "생성: " ++ todo ^. todoCreatedAt)

drawDetailView :: AppState -> Widget Name
drawDetailView s =
  case s ^. mode of
    ViewMode ->
      case listSelected (s ^. todoList) of
        Nothing ->
          borderWithLabel (str " 상세 정보 ") <|
            padAll 1 <|
              center <| str "선택된 할일이 없습니다"
        Just idx ->
          let todos = s ^. todoList . listElementsL
          in case todos Vec.!? idx of
            Nothing ->
              borderWithLabel (str " 상세 정보 ") <|
                padAll 1 <|
                  center <| str "선택된 할일이 없습니다"
            Just todo ->
              let statusText = if todo ^. todoCompleted then "✓ 완료됨" else "○ 진행중"
                  statusAttr = if todo ^. todoCompleted
                                 then attrName "completed"
                                 else attrName "normal"

                  showDetailField _ Nothing = str ""
                  showDetailField lbl (Just val) =
                    hBox [withAttr (attrName "detailLabel") <| str (lbl ++ ": "),
                          str val]

                  completedInfo = case todo ^. todoCompletedAt of
                    Just compTime ->
                      hBox [withAttr (attrName "detailLabel") <| str "완료 시각: ",
                            withAttr (attrName "timestamp") <| str compTime]
                    Nothing -> str ""

              in borderWithLabel (str " 상세 정보 ") <|
                   padAll 1 <|
                     vLimit 8 <|
                       vBox
                         [ hBox [withAttr (attrName "detailLabel") <| str "ID: ",
                                 str (show (todo ^. todoId))]
                         , hBox [withAttr (attrName "detailLabel") <| str "상태: ",
                                 withAttr statusAttr <| str statusText]
                         , hBox [withAttr (attrName "detailLabel") <| str "할일: ",
                                 str (todo ^. todoAction)]
                         , showDetailField "주체자" (todo ^. todoSubject)
                         , showDetailField "대상자" (todo ^. todoIndirectObject)
                         , showDetailField "작업대상" (todo ^. todoDirectObject)
                         , hBox [withAttr (attrName "detailLabel") <| str "생성 시각: ",
                                 withAttr (attrName "timestamp") <| str (todo ^. todoCreatedAt)]
                         , completedInfo
                         ]
    EditMode _ ->
      case s ^. editingIndex of
        Nothing ->
          borderWithLabel (str " 상세 정보 (편집 모드) ") <|
            padAll 1 <|
              center <| str "편집할 항목을 찾을 수 없습니다"
        Just idx ->
          let todos = s ^. todoList . listElementsL
          in case todos Vec.!? idx of
            Nothing ->
              borderWithLabel (str " 상세 정보 (편집 모드) ") <|
                padAll 1 <|
                  center <| str "편집할 항목을 찾을 수 없습니다"
            Just todo ->
              let statusText = if todo ^. todoCompleted then "✓ 완료됨" else "○ 진행중"
                  statusAttr = if todo ^. todoCompleted
                                 then attrName "completed"
                                 else attrName "normal"

                  -- 각 필드의 포커스 상태
                  actionFocused = s ^. focusedField == FocusAction
                  subjectFocused = s ^. focusedField == FocusSubject
                  indirectObjFocused = s ^. focusedField == FocusIndirectObject
                  directObjFocused = s ^. focusedField == FocusDirectObject

                  -- 편집 가능한 필드 렌더링
                  renderEditField fieldLabel editor isFocused =
                    let fieldAttr = if isFocused
                                      then attrName "focusedField"
                                      else attrName "detailLabel"
                        labelWidget = withAttr fieldAttr <| str (fieldLabel ++ ": ")
                        editorWidget = E.renderEditor (str . unlines) isFocused editor
                    in hBox [labelWidget, editorWidget]

                  completedInfo = case todo ^. todoCompletedAt of
                    Just compTime ->
                      hBox [withAttr (attrName "detailLabel") <| str "완료 시각: ",
                            withAttr (attrName "timestamp") <| str compTime]
                    Nothing -> str ""

              in borderWithLabel (str " 상세 정보 (편집 모드 - Tab: 다음 필드, Enter: 저장, Esc: 취소) ") <|
                   padAll 1 <|
                     vLimit 8 <|
                       vBox
                         [ hBox [withAttr (attrName "detailLabel") <| str "ID: ",
                                 str (show (todo ^. todoId))]
                         , hBox [withAttr (attrName "detailLabel") <| str "상태: ",
                                 withAttr statusAttr <| str statusText]
                         , renderEditField "할일 (필수)" (s ^. actionEditor) actionFocused
                         , renderEditField "주체자" (s ^. subjectEditor) subjectFocused
                         , renderEditField "대상자" (s ^. indirectObjectEditor) indirectObjFocused
                         , renderEditField "작업대상" (s ^. directObjectEditor) directObjFocused
                         , hBox [withAttr (attrName "detailLabel") <| str "생성 시각: ",
                                 withAttr (attrName "timestamp") <| str (todo ^. todoCreatedAt)]
                         , completedInfo
                         ]
    InputMode ->
      let -- 각 필드의 포커스 상태
          actionFocused = s ^. focusedField == FocusAction
          subjectFocused = s ^. focusedField == FocusSubject
          indirectObjFocused = s ^. focusedField == FocusIndirectObject
          directObjFocused = s ^. focusedField == FocusDirectObject

          -- 편집 가능한 필드 렌더링
          renderEditField fieldLabel editor isFocused =
            let fieldAttr = if isFocused
                              then attrName "focusedField"
                              else attrName "detailLabel"
                labelWidget = withAttr fieldAttr <| str (fieldLabel ++ ": ")
                editorWidget = E.renderEditor (str . unlines) isFocused editor
            in hBox [labelWidget, editorWidget]

       in borderWithLabel (str " 새 할일 추가 (Tab: 다음 필드, Enter: 저장, Esc: 취소) ") <|
            padAll 1 <|
              vLimit 8 <|
                vBox
                  [ hBox [withAttr (attrName "detailLabel") <| str "ID: ",
                          withAttr (attrName "timestamp") <| str "(자동 생성)"]
                  , hBox [withAttr (attrName "detailLabel") <| str "상태: ",
                          str "○ 진행중"]
                  , renderEditField "할일 (필수)" (s ^. actionEditor) actionFocused
                  , renderEditField "주체자" (s ^. subjectEditor) subjectFocused
                  , renderEditField "대상자" (s ^. indirectObjectEditor) indirectObjFocused
                  , renderEditField "작업대상" (s ^. directObjectEditor) directObjFocused
                  , hBox [withAttr (attrName "detailLabel") <| str "생성 시각: ",
                          withAttr (attrName "timestamp") <| str "(자동 생성)"]
                  , str ""
                  ]

drawHelp :: AppState -> Widget Name
drawHelp s =
  padAll 1 <|
    case s ^. mode of
      InputMode -> str "Tab: 다음 필드 | Enter: 저장 | Esc: 취소"
      EditMode _ -> str "Tab: 다음 필드 | Enter: 저장 | Esc: 취소"
      ViewMode ->
        let kb = s ^. keyBindings
            quitKeys = fromMaybe "q" (listToMaybe (Config.quit kb))
            addKeys = fromMaybe "a" (listToMaybe (Config.add_todo kb))
            toggleKeys = fromMaybe "t" (listToMaybe (Config.toggle_complete kb))
            deleteKeys = fromMaybe "d" (listToMaybe (Config.delete_todo kb))
            upKeys = fromMaybe "k" (listToMaybe (Config.navigate_up kb))
            downKeys = fromMaybe "j" (listToMaybe (Config.navigate_down kb))
        in vBox
          [ str <| addKeys ++ ": Add | e: Edit | " ++ toggleKeys ++ ": Toggle | "
                  ++ deleteKeys ++ ": Delete | " ++ upKeys ++ "/" ++ downKeys
                  ++ ": Navigate | " ++ quitKeys ++ ": Quit"
          ]

-- 이벤트 처리
handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent ev = do
  s <- get
  case s ^. mode of
    ViewMode   -> handleViewMode ev
    InputMode  -> handleInputMode ev
    EditMode _ -> handleEditMode ev

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
        Nothing -> pure ()
        Just idx -> do
          let todos = s' ^. todoList . listElementsL
          case todos Vec.!? idx of
            Nothing -> pure ()
            Just todo -> do
              let tid = todo ^. todoId
                  conn = s' ^. dbConn
                  env = App.AppEnv conn
              liftIO <| App.runAppM env (App.toggleTodo tid)
              -- Reload updated todo from DB
              updatedRows <- liftIO <| App.runAppM env App.loadTodos
              case find (\row -> DB.todoId row == tid) updatedRows of
                Just row ->
                  modify <| todoList %~ listModify (\t -> t
                    { _todoCompleted = DB.todoCompleted row
                    , _todoCompletedAt = DB.todoCompletedAt row
                    })
                Nothing -> pure ()
    Just Config.DeleteTodo -> do
      s' <- get
      case listSelected (s' ^. todoList) of
        Nothing  -> pure ()
        Just idx -> do
          let todos = s' ^. todoList . listElementsL
          case todos Vec.!? idx of
            Nothing -> pure ()
            Just todo -> do
              let tid = todo ^. todoId
                  conn = s' ^. dbConn
              liftIO <| App.runAppM (App.AppEnv conn) (App.deleteTodo tid)
              modify <| todoList %~ listRemove idx
    Just Config.NavigateUp -> zoom todoList <| handleListEvent (V.EvKey V.KUp [])
    Just Config.NavigateDown -> zoom todoList <| handleListEvent (V.EvKey V.KDown [])
    _ -> case key of
      V.KChar 'e' -> do
        s' <- get
        case listSelected (s' ^. todoList) of
          Nothing -> pure ()
          Just idx -> do
            let todos = s' ^. todoList . listElementsL
            case todos Vec.!? idx of
              Nothing -> pure ()
              Just todo -> do
                modify <| (mode .~ EditMode (todo ^. todoId))
                       . (editingIndex .~ Just idx)
                       . (focusedField .~ FocusAction)
                       . (actionEditor .~ E.editor ActionField (Just 1) (todo ^. todoAction))
                       . (subjectEditor .~ E.editor SubjectField (Just 1) (fromMaybe "" <| todo ^. todoSubject))
                       . (indirectObjectEditor .~ E.editor IndirectObjectField (Just 1) (fromMaybe "" <| todo ^. todoIndirectObject))
                       . (directObjectEditor .~ E.editor DirectObjectField (Just 1) (fromMaybe "" <| todo ^. todoDirectObject))
      _ -> pure ()
handleViewMode _ = pure ()

handleInputMode :: BrickEvent Name e -> EventM Name AppState ()
handleInputMode (VtyEvent (V.EvKey key [])) = do
  s <- get
  let kb = s ^. keyBindings
  case Config.matchesKey kb key of
    Just Config.CancelInput -> clearEditorsAndReturnToView
    Just Config.SaveInput -> do
      s' <- get
      let action      = trim <| unlines <| E.getEditContents (s' ^. actionEditor)
          subject     = trim <| unlines <| E.getEditContents (s' ^. subjectEditor)
          indirectObj = trim <| unlines <| E.getEditContents (s' ^. indirectObjectEditor)
          directObj   = trim <| unlines <| E.getEditContents (s' ^. directObjectEditor)

          toMaybe str = if null str then Nothing else Just str

      if not (null action)
        then do
          let conn = s' ^. dbConn
              env = App.AppEnv conn
          newTodoRow <- liftIO <| App.runAppM env <| do
            tid <- App.createTodoWithFields action (toMaybe subject) (toMaybe indirectObj) (toMaybe directObj)
            rows <- App.loadTodos
            pure <| find (\row -> DB.todoId row == tid) rows

          case newTodoRow of
            Just row -> do
              let newTodo = fromTodoRow row
              modify <| (todoList %~ listInsert 0 newTodo)
                     . (mode .~ ViewMode)
              clearEditors
            Nothing -> modify <| mode .~ ViewMode
        else
          modify <| mode .~ ViewMode
    _ -> case key of
      V.KChar '\t' -> do
        s' <- get
        let nextField = case s' ^. focusedField of
              FocusAction         -> FocusSubject
              FocusSubject        -> FocusIndirectObject
              FocusIndirectObject -> FocusDirectObject
              FocusDirectObject   -> FocusAction
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
    FocusAction         -> zoom actionEditor <| E.handleEditorEvent ev
    FocusSubject        -> zoom subjectEditor <| E.handleEditorEvent ev
    FocusIndirectObject -> zoom indirectObjectEditor <| E.handleEditorEvent ev
    FocusDirectObject   -> zoom directObjectEditor <| E.handleEditorEvent ev
handleInputMode _ = return ()

handleEditMode :: BrickEvent Name e -> EventM Name AppState ()
handleEditMode (VtyEvent (V.EvKey key [])) = do
  s <- get
  let kb = s ^. keyBindings
  case Config.matchesKey kb key of
    Just Config.CancelInput -> do
      modify <| (mode .~ ViewMode) . (editingIndex .~ Nothing)
      clearEditors
    Just Config.SaveInput -> do
      s' <- get
      let action      = trim <| unlines <| E.getEditContents (s' ^. actionEditor)
          subject     = trim <| unlines <| E.getEditContents (s' ^. subjectEditor)
          indirectObj = trim <| unlines <| E.getEditContents (s' ^. indirectObjectEditor)
          directObj   = trim <| unlines <| E.getEditContents (s' ^. directObjectEditor)

          toMaybe str = if null str then Nothing else Just str

      case s' ^. mode of
        EditMode tid -> do
          when (not <| null action) <| do
            let conn = s' ^. dbConn
                env = App.AppEnv conn
            case s' ^. editingIndex of
              Nothing -> pure ()
              Just idx -> do
                let todos = s' ^. todoList . listElementsL
                case todos Vec.!? idx of
                  Nothing -> pure ()
                  Just oldTodo -> do
                    liftIO <| App.runAppM env $
                      App.updateTodo tid action (toMaybe subject) (toMaybe indirectObj) (toMaybe directObj)

                    let updatedTodo = oldTodo
                          { _todoAction = action
                          , _todoSubject = toMaybe subject
                          , _todoIndirectObject = toMaybe indirectObj
                          , _todoDirectObject = toMaybe directObj
                          }
                    modify <| todoList %~ listModify (const updatedTodo)

          modify <| (mode .~ ViewMode) . (editingIndex .~ Nothing)
          clearEditors
        _ -> pure ()
    _ -> case key of
      V.KChar '\t' -> do
        s' <- get
        let nextField = case s' ^. focusedField of
              FocusAction         -> FocusSubject
              FocusSubject        -> FocusIndirectObject
              FocusIndirectObject -> FocusDirectObject
              FocusDirectObject   -> FocusAction
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
    FocusAction         -> zoom actionEditor <| E.handleEditorEvent ev
    FocusSubject        -> zoom subjectEditor <| E.handleEditorEvent ev
    FocusIndirectObject -> zoom indirectObjectEditor <| E.handleEditorEvent ev
    FocusDirectObject   -> zoom directObjectEditor <| E.handleEditorEvent ev
handleEditMode _ = return ()

-- Utility functions
trim :: String -> String
trim = unwords . words

clearEditors :: EventM Name AppState ()
clearEditors = modify $
    (actionEditor .~ E.editor ActionField (Just 1) "")
  . (subjectEditor .~ E.editor SubjectField (Just 1) "")
  . (indirectObjectEditor .~ E.editor IndirectObjectField (Just 1) "")
  . (directObjectEditor .~ E.editor DirectObjectField (Just 1) "")

clearEditorsAndReturnToView :: EventM Name AppState ()
clearEditorsAndReturnToView = do
    modify <| mode .~ ViewMode
    clearEditors

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
      (attrName "detailLabel", fg V.cyan `V.withStyle` V.bold),
      (listSelectedAttr, V.black `on` V.cyan)
    ]

-- 애플리케이션 정의
app :: App AppState e Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = \s locs -> case s ^. mode of
        InputMode -> case s ^. focusedField of
          FocusAction         -> showCursorNamed ActionField locs
          FocusSubject        -> showCursorNamed SubjectField locs
          FocusIndirectObject -> showCursorNamed IndirectObjectField locs
          FocusDirectObject   -> showCursorNamed DirectObjectField locs
        EditMode _ -> case s ^. focusedField of
          FocusAction         -> showCursorNamed ActionField locs
          FocusSubject        -> showCursorNamed SubjectField locs
          FocusIndirectObject -> showCursorNamed IndirectObjectField locs
          FocusDirectObject   -> showCursorNamed DirectObjectField locs
        ViewMode  -> Nothing,
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap = const theMap
    }

tuiMain :: IO ()
tuiMain = do
  kb <- Config.loadKeyBindings "config/keybindings.yaml"

  conn <- open "todos.db"
  DB.initDB conn

  let env = App.AppEnv conn
  todoRows <- App.runAppM env App.loadTodos

  let initialTodos = Vec.fromList <| map fromTodoRow todoRows
      initialState = AppState
          { _todoList             = list TodoList initialTodos 1
          , _actionEditor         = E.editor ActionField (Just 1) ""
          , _subjectEditor        = E.editor SubjectField (Just 1) ""
          , _indirectObjectEditor = E.editor IndirectObjectField (Just 1) ""
          , _directObjectEditor   = E.editor DirectObjectField (Just 1) ""
          , _focusedField         = FocusAction
          , _mode                 = ViewMode
          , _dbConn               = conn
          , _keyBindings          = kb
          , _editingIndex         = Nothing
          }

  void <| defaultMain app initialState
