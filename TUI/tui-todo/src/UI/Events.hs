{-# LANGUAGE OverloadedStrings #-}

module UI.Events
    ( handleEvent
    , trim
    ) where

import           Brick                  (BrickEvent (VtyEvent), EventM, get,
                                         halt, modify, zoom)
import qualified Brick.Widgets.Edit     as E
import           Brick.Widgets.List     (listModify, listRemove, listSelected,
                                         handleListEvent, listElementsL,
                                         listInsert)

import qualified App
import qualified Config
import           Control.Monad          (when)
import           Control.Monad.IO.Class (liftIO)
import qualified DB
import           Data.List              (find)
import           Data.Maybe             (fromMaybe)
import qualified Data.Vector            as Vec
import qualified Graphics.Vty           as V
import           Lens.Micro             ((%~), (.~), (^.))

import           UI.Types

-- | 이벤트 처리
handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent ev = do
  s <- get
  case s ^. mode of
    ViewMode   -> handleViewMode ev
    InputMode  -> handleInputMode ev
    EditMode _ -> handleEditMode ev

-- | ViewMode 이벤트 처리
handleViewMode :: BrickEvent Name e -> EventM Name AppState ()
handleViewMode (VtyEvent (V.EvKey key [])) = do
  s <- get
  let kb = s ^. keyBindings
  case Config.matchesKey kb key of
    Just Config.QuitApp        -> halt
    Just Config.AddTodo        -> enterInputMode
    Just Config.ToggleComplete -> toggleTodoComplete
    Just Config.DeleteTodo     -> deleteTodo
    Just Config.NavigateUp     -> zoom todoList $ handleListEvent (V.EvKey V.KUp [])
    Just Config.NavigateDown   -> zoom todoList $ handleListEvent (V.EvKey V.KDown [])
    _                          -> handleEditKey key
handleViewMode _ = pure ()

-- | InputMode로 전환
enterInputMode :: EventM Name AppState ()
enterInputMode = do
  modify $ mode .~ InputMode
  modify $ focusedField .~ FocusAction

-- | Todo 완료 상태 토글
toggleTodoComplete :: EventM Name AppState ()
toggleTodoComplete = do
  s <- get
  case listSelected (s ^. todoList) of
    Nothing -> pure ()
    Just idx -> do
      let todos = s ^. todoList . listElementsL
      case todos Vec.!? idx of
        Nothing   -> pure ()
        Just todo -> toggleTodoInDB s (todo ^. todoId) idx

-- | DB에서 Todo 토글
toggleTodoInDB :: AppState -> DB.TodoId -> Int -> EventM Name AppState ()
toggleTodoInDB s tid _idx = do
  let conn = s ^. dbConn
      msgs = s ^. i18nMessages
      env = App.AppEnv conn msgs
  liftIO $ App.runAppM env (App.toggleTodo tid)
  updatedRows <- liftIO $ App.runAppM env App.loadTodos
  case find (\row -> DB.todoId row == tid) updatedRows of
    Just row ->
      modify $
        todoList
          %~ listModify
            ( \t ->
                t
                  { _todoCompleted = DB.todoCompleted row,
                    _todoCompletedAt = DB.todoCompletedAt row
                  }
            )
    Nothing -> pure ()

-- | Todo 삭제
deleteTodo :: EventM Name AppState ()
deleteTodo = do
  s <- get
  case listSelected (s ^. todoList) of
    Nothing -> pure ()
    Just idx -> do
      let todos = s ^. todoList . listElementsL
      case todos Vec.!? idx of
        Nothing   -> pure ()
        Just todo -> deleteTodoFromDB s (todo ^. todoId) idx

-- | DB에서 Todo 삭제
deleteTodoFromDB :: AppState -> DB.TodoId -> Int -> EventM Name AppState ()
deleteTodoFromDB s tid idx = do
  let conn = s ^. dbConn
      msgs = s ^. i18nMessages
  liftIO $ App.runAppM (App.AppEnv conn msgs) (App.deleteTodo tid)
  modify $ todoList %~ listRemove idx

-- | 편집 키 처리
handleEditKey :: V.Key -> EventM Name AppState ()
handleEditKey (V.KChar 'e') = do
  s <- get
  case listSelected (s ^. todoList) of
    Nothing -> pure ()
    Just idx -> do
      let todos = s ^. todoList . listElementsL
      case todos Vec.!? idx of
        Nothing   -> pure ()
        Just todo -> enterEditMode todo idx
handleEditKey _ = pure ()

-- | EditMode로 전환
enterEditMode :: Todo -> Int -> EventM Name AppState ()
enterEditMode todo idx = do
  modify $
    (mode .~ EditMode (todo ^. todoId))
      . (editingIndex .~ Just idx)
      . (focusedField .~ FocusAction)
      . (actionEditor .~ E.editor ActionField (Just 1) (todo ^. todoAction))
      . (subjectEditor .~ E.editor SubjectField (Just 1) (fromMaybe "" $ todo ^. todoSubject))
      . (indirectObjectEditor .~ E.editor IndirectObjectField (Just 1) (fromMaybe "" $ todo ^. todoIndirectObject))
      . (directObjectEditor .~ E.editor DirectObjectField (Just 1) (fromMaybe "" $ todo ^. todoDirectObject))

-- | InputMode 이벤트 처리
handleInputMode :: BrickEvent Name e -> EventM Name AppState ()
handleInputMode (VtyEvent (V.EvKey key [])) = do
  s <- get
  let kb = s ^. keyBindings
  case Config.matchesKey kb key of
    Just Config.CancelInput -> clearEditorsAndReturnToView
    Just Config.SaveInput   -> saveNewTodo
    _                       -> handleInputModeKey key
handleInputMode ev@(VtyEvent _) = handleEditorEvent ev
handleInputMode _ = return ()

-- | 새 Todo 저장
saveNewTodo :: EventM Name AppState ()
saveNewTodo = do
  s <- get
  let action = trim $ unlines $ E.getEditContents (s ^. actionEditor)
      subject = trim $ unlines $ E.getEditContents (s ^. subjectEditor)
      indirectObj = trim $ unlines $ E.getEditContents (s ^. indirectObjectEditor)
      directObj = trim $ unlines $ E.getEditContents (s ^. directObjectEditor)
      toMaybe txt = if null txt then Nothing else Just txt

  if not (null action)
    then createAndInsertTodo s action (toMaybe subject) (toMaybe indirectObj) (toMaybe directObj)
    else modify $ mode .~ ViewMode

-- | Todo 생성 및 삽입
createAndInsertTodo :: AppState -> String -> Maybe String -> Maybe String -> Maybe String -> EventM Name AppState ()
createAndInsertTodo s action subject indirectObj directObj = do
  let conn = s ^. dbConn
      msgs = s ^. i18nMessages
      env = App.AppEnv conn msgs
  newTodoRow <-
    liftIO $ App.runAppM env $ do
      tid <- App.createTodoWithFields action subject indirectObj directObj
      rows <- App.loadTodos
      pure $ find (\row -> DB.todoId row == tid) rows

  case newTodoRow of
    Just row -> do
      let newTodo = fromTodoRow row
      modify $
        (todoList %~ listInsert 0 newTodo)
          . (mode .~ ViewMode)
      clearEditors
    Nothing -> modify $ mode .~ ViewMode

-- | InputMode 키 처리
handleInputModeKey :: V.Key -> EventM Name AppState ()
handleInputModeKey (V.KChar '\t') = cycleFieldFocus
handleInputModeKey key = do
  s <- get
  case s ^. focusedField of
    FocusAction         -> zoom actionEditor $ E.handleEditorEvent (VtyEvent (V.EvKey key []))
    FocusSubject        -> zoom subjectEditor $ E.handleEditorEvent (VtyEvent (V.EvKey key []))
    FocusIndirectObject -> zoom indirectObjectEditor $ E.handleEditorEvent (VtyEvent (V.EvKey key []))
    FocusDirectObject   -> zoom directObjectEditor $ E.handleEditorEvent (VtyEvent (V.EvKey key []))

-- | EditMode 이벤트 처리
handleEditMode :: BrickEvent Name e -> EventM Name AppState ()
handleEditMode (VtyEvent (V.EvKey key [])) = do
  s <- get
  let kb = s ^. keyBindings
  case Config.matchesKey kb key of
    Just Config.CancelInput -> cancelEdit
    Just Config.SaveInput   -> saveEditedTodo
    _                       -> handleEditModeKey key
handleEditMode ev@(VtyEvent _) = handleEditorEvent ev
handleEditMode _ = return ()

-- | 편집 취소
cancelEdit :: EventM Name AppState ()
cancelEdit = do
  modify $ (mode .~ ViewMode) . (editingIndex .~ Nothing)
  clearEditors

-- | 편집된 Todo 저장
saveEditedTodo :: EventM Name AppState ()
saveEditedTodo = do
  s <- get
  let action = trim $ unlines $ E.getEditContents (s ^. actionEditor)
      subject = trim $ unlines $ E.getEditContents (s ^. subjectEditor)
      indirectObj = trim $ unlines $ E.getEditContents (s ^. indirectObjectEditor)
      directObj = trim $ unlines $ E.getEditContents (s ^. directObjectEditor)
      toMaybe txt = if null txt then Nothing else Just txt

  case s ^. mode of
    EditMode tid -> do
      when (not $ null action) $
        updateTodoInDB s tid action (toMaybe subject) (toMaybe indirectObj) (toMaybe directObj)
      modify $ (mode .~ ViewMode) . (editingIndex .~ Nothing)
      clearEditors
    _ -> pure ()

-- | DB에서 Todo 업데이트
updateTodoInDB :: AppState -> DB.TodoId -> String -> Maybe String -> Maybe String -> Maybe String -> EventM Name AppState ()
updateTodoInDB s tid action subject indirectObj directObj = do
  let conn = s ^. dbConn
      msgs = s ^. i18nMessages
      env = App.AppEnv conn msgs
  case s ^. editingIndex of
    Nothing -> pure ()
    Just idx -> do
      let todos = s ^. todoList . listElementsL
      case todos Vec.!? idx of
        Nothing      -> pure ()
        Just oldTodo -> do
          liftIO $
            App.runAppM env $
              App.updateTodo tid action subject indirectObj directObj

          let updatedTodo =
                oldTodo
                  { _todoAction = action,
                    _todoSubject = subject,
                    _todoIndirectObject = indirectObj,
                    _todoDirectObject = directObj
                  }
          modify $ todoList %~ listModify (const updatedTodo)

-- | EditMode 키 처리
handleEditModeKey :: V.Key -> EventM Name AppState ()
handleEditModeKey (V.KChar '\t') = cycleFieldFocus
handleEditModeKey key = do
  s <- get
  case s ^. focusedField of
    FocusAction         -> zoom actionEditor $ E.handleEditorEvent (VtyEvent (V.EvKey key []))
    FocusSubject        -> zoom subjectEditor $ E.handleEditorEvent (VtyEvent (V.EvKey key []))
    FocusIndirectObject -> zoom indirectObjectEditor $ E.handleEditorEvent (VtyEvent (V.EvKey key []))
    FocusDirectObject   -> zoom directObjectEditor $ E.handleEditorEvent (VtyEvent (V.EvKey key []))

-- | 필드 포커스 순환
cycleFieldFocus :: EventM Name AppState ()
cycleFieldFocus = do
  s <- get
  let nextField = case s ^. focusedField of
        FocusAction         -> FocusSubject
        FocusSubject        -> FocusIndirectObject
        FocusIndirectObject -> FocusDirectObject
        FocusDirectObject   -> FocusAction
  modify $ focusedField .~ nextField

-- | 에디터 이벤트 처리
handleEditorEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEditorEvent ev = do
  s <- get
  case s ^. focusedField of
    FocusAction         -> zoom actionEditor $ E.handleEditorEvent ev
    FocusSubject        -> zoom subjectEditor $ E.handleEditorEvent ev
    FocusIndirectObject -> zoom indirectObjectEditor $ E.handleEditorEvent ev
    FocusDirectObject   -> zoom directObjectEditor $ E.handleEditorEvent ev

-- | Utility: 문자열 trim
trim :: String -> String
trim = unwords . words

-- | 에디터 초기화
clearEditors :: EventM Name AppState ()
clearEditors =
  modify $
    (actionEditor .~ E.editor ActionField (Just 1) "")
      . (subjectEditor .~ E.editor SubjectField (Just 1) "")
      . (indirectObjectEditor .~ E.editor IndirectObjectField (Just 1) "")
      . (directObjectEditor .~ E.editor DirectObjectField (Just 1) "")

-- | 에디터 초기화 및 ViewMode로 복귀
clearEditorsAndReturnToView :: EventM Name AppState ()
clearEditorsAndReturnToView = do
  modify $ mode .~ ViewMode
  clearEditors
