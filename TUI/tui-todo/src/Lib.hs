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
    , i18nMessages
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
    , tuiMainWithLanguage
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

import qualified I18n

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
                         , _i18nMessages         :: !I18n.I18nMessages
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
        [ drawHeader s,
          hBorder,
          drawTodoList s,
          hBorder,
          drawDetailView s,
          hBorder,
          drawHelp s
        ]

drawHeader :: AppState -> Widget Name
drawHeader s =
  withAttr (attrName "header") <|
    hCenter <|
      padTopBottom 1 <|
        str <| I18n.header (I18n.ui (s ^. i18nMessages))

drawTodoList :: AppState -> Widget Name
drawTodoList s =
  let msgs = s ^. i18nMessages
      uiMsgs = I18n.ui msgs
  in borderWithLabel (str <| I18n.todos_title uiMsgs) <|
       padAll 1 <|
         vLimit 20 <|
           if null (s ^. todoList . listElementsL)
             then center <| str <| I18n.no_todos uiMsgs
             else renderList (drawTodo msgs) True (s ^. todoList)

drawTodo :: I18n.I18nMessages -> Bool -> Todo -> Widget Name
drawTodo msgs selected todo = withAttr selectAttr <| hBox [checkbox, str mainInfo, timestamp]
  where
    listMsgs = I18n.list msgs
    
    checkbox = str <| if todo ^. todoCompleted
                        then I18n.checkbox_done listMsgs
                        else I18n.checkbox_todo listMsgs

    todoAttr = attrName <| if todo ^. todoCompleted then "completed" else "normal"
    selectAttr = if selected then attrName "selected" else todoAttr

    showField _ Nothing    = ""
    showField lbl (Just v) = I18n.field_separator listMsgs ++ lbl ++ ": " ++ v

    mainInfo = concat
        [ I18n.field_action listMsgs ++ ": " ++ todo ^. todoAction
        , showField (I18n.field_subject listMsgs) (todo ^. todoSubject)
        , showField (I18n.field_indirect listMsgs) (todo ^. todoIndirectObject)
        , showField (I18n.field_direct listMsgs) (todo ^. todoDirectObject)
        ]

    completedTimeText = maybe "" (\t -> I18n.completed_prefix listMsgs ++ t ++ I18n.field_separator listMsgs) (todo ^. todoCompletedAt)

    timestamp = padLeft Max <| withAttr (attrName "timestamp") $
        str (completedTimeText ++ I18n.created_prefix listMsgs ++ todo ^. todoCreatedAt)

drawDetailView :: AppState -> Widget Name
drawDetailView s =
  let msgs = s ^. i18nMessages
      uiMsgs = I18n.ui msgs
      fieldMsgs = I18n.fields msgs
      statusMsgs = I18n.status msgs
  in case s ^. mode of
    ViewMode ->
      case listSelected (s ^. todoList) of
        Nothing ->
          borderWithLabel (str <| I18n.detail_title uiMsgs) <|
            padAll 1 <|
              center <| str <| I18n.no_selection uiMsgs
        Just idx ->
          let todos = s ^. todoList . listElementsL
          in case todos Vec.!? idx of
            Nothing ->
              borderWithLabel (str <| I18n.detail_title uiMsgs) <|
                padAll 1 <|
                  center <| str <| I18n.no_selection uiMsgs
            Just todo ->
              let statusText = if todo ^. todoCompleted
                                 then I18n.completed statusMsgs
                                 else I18n.in_progress statusMsgs
                  statusAttr = if todo ^. todoCompleted
                                 then attrName "completed"
                                 else attrName "normal"

                  showDetailField _ Nothing = str ""
                  showDetailField lbl (Just val) =
                    hBox [withAttr (attrName "detailLabel") <| str (lbl ++ ": "),
                          str val]

                  completedInfo = case todo ^. todoCompletedAt of
                    Just compTime ->
                      hBox [withAttr (attrName "detailLabel") <| str (I18n.completed_at_label fieldMsgs ++ ": "),
                            withAttr (attrName "timestamp") <| str compTime]
                    Nothing -> str ""

              in borderWithLabel (str <| I18n.detail_title uiMsgs) <|
                   padAll 1 <|
                     vLimit 8 <|
                       vBox
                         [ hBox [withAttr (attrName "detailLabel") <| str (I18n.id_label fieldMsgs ++ ": "),
                                 str (show (todo ^. todoId))]
                         , hBox [withAttr (attrName "detailLabel") <| str (I18n.status_label fieldMsgs ++ ": "),
                                 withAttr statusAttr <| str statusText]
                         , hBox [withAttr (attrName "detailLabel") <| str (I18n.action_label fieldMsgs ++ ": "),
                                 str (todo ^. todoAction)]
                         , showDetailField (I18n.subject_label fieldMsgs) (todo ^. todoSubject)
                         , showDetailField (I18n.indirect_object_label fieldMsgs) (todo ^. todoIndirectObject)
                         , showDetailField (I18n.direct_object_label fieldMsgs) (todo ^. todoDirectObject)
                         , hBox [withAttr (attrName "detailLabel") <| str (I18n.created_at_label fieldMsgs ++ ": "),
                                 withAttr (attrName "timestamp") <| str (todo ^. todoCreatedAt)]
                         , completedInfo
                         ]
    EditMode _ ->
      case s ^. editingIndex of
        Nothing ->
          borderWithLabel (str <| I18n.detail_edit_title uiMsgs) <|
            padAll 1 <|
              center <| str <| I18n.not_found uiMsgs
        Just idx ->
          let todos = s ^. todoList . listElementsL
          in case todos Vec.!? idx of
            Nothing ->
              borderWithLabel (str <| I18n.detail_edit_title uiMsgs) <|
                padAll 1 <|
                  center <| str <| I18n.not_found uiMsgs
            Just todo ->
              let statusText = if todo ^. todoCompleted
                                 then I18n.completed statusMsgs
                                 else I18n.in_progress statusMsgs
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
                      hBox [withAttr (attrName "detailLabel") <| str (I18n.completed_at_label fieldMsgs ++ ": "),
                            withAttr (attrName "timestamp") <| str compTime]
                    Nothing -> str ""

              in borderWithLabel (str <| I18n.detail_edit_title uiMsgs) <|
                   padAll 1 <|
                     vLimit 8 <|
                       vBox
                         [ hBox [withAttr (attrName "detailLabel") <| str (I18n.id_label fieldMsgs ++ ": "),
                                 str (show (todo ^. todoId))]
                         , hBox [withAttr (attrName "detailLabel") <| str (I18n.status_label fieldMsgs ++ ": "),
                                 withAttr statusAttr <| str statusText]
                         , renderEditField (I18n.action_required_label fieldMsgs) (s ^. actionEditor) actionFocused
                         , renderEditField (I18n.subject_label fieldMsgs) (s ^. subjectEditor) subjectFocused
                         , renderEditField (I18n.indirect_object_label fieldMsgs) (s ^. indirectObjectEditor) indirectObjFocused
                         , renderEditField (I18n.direct_object_label fieldMsgs) (s ^. directObjectEditor) directObjFocused
                         , hBox [withAttr (attrName "detailLabel") <| str (I18n.created_at_label fieldMsgs ++ ": "),
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

       in borderWithLabel (str <| I18n.detail_add_title uiMsgs) <|
            padAll 1 <|
              vLimit 8 <|
                vBox
                  [ hBox [withAttr (attrName "detailLabel") <| str (I18n.id_label fieldMsgs ++ ": "),
                          withAttr (attrName "timestamp") <| str <| I18n.auto_generated_label fieldMsgs]
                  , hBox [withAttr (attrName "detailLabel") <| str (I18n.status_label fieldMsgs ++ ": "),
                          str <| I18n.in_progress statusMsgs]
                  , renderEditField (I18n.action_required_label fieldMsgs) (s ^. actionEditor) actionFocused
                  , renderEditField (I18n.subject_label fieldMsgs) (s ^. subjectEditor) subjectFocused
                  , renderEditField (I18n.indirect_object_label fieldMsgs) (s ^. indirectObjectEditor) indirectObjFocused
                  , renderEditField (I18n.direct_object_label fieldMsgs) (s ^. directObjectEditor) directObjFocused
                  , hBox [withAttr (attrName "detailLabel") <| str (I18n.created_at_label fieldMsgs ++ ": "),
                          withAttr (attrName "timestamp") <| str <| I18n.auto_generated_label fieldMsgs]
                  , str ""
                  ]

drawHelp :: AppState -> Widget Name
drawHelp s =
  let msgs = s ^. i18nMessages
      helpMsgs = I18n.help msgs
  in padAll 1 <|
       case s ^. mode of
         InputMode -> str <| I18n.input_mode helpMsgs
         EditMode _ -> str <| I18n.edit_mode helpMsgs
         ViewMode ->
           let kb = s ^. keyBindings
               quitKeys = fromMaybe "q" (listToMaybe (Config.quit kb))
               addKeys = fromMaybe "a" (listToMaybe (Config.add_todo kb))
               toggleKeys = fromMaybe "t" (listToMaybe (Config.toggle_complete kb))
               deleteKeys = fromMaybe "d" (listToMaybe (Config.delete_todo kb))
               upKeys = fromMaybe "k" (listToMaybe (Config.navigate_up kb))
               downKeys = fromMaybe "j" (listToMaybe (Config.navigate_down kb))
               
               addLabel = I18n.add helpMsgs
               editLabel = I18n.edit helpMsgs
               toggleLabel = I18n.toggle helpMsgs
               deleteLabel = I18n.delete helpMsgs
               navigateLabel = I18n.navigate helpMsgs
               quitLabel = I18n.quit helpMsgs
           in vBox
             [ str <| addKeys ++ ": " ++ addLabel ++ " | e: " ++ editLabel ++ " | " 
                     ++ toggleKeys ++ ": " ++ toggleLabel ++ " | "
                     ++ deleteKeys ++ ": " ++ deleteLabel ++ " | " 
                     ++ upKeys ++ "/" ++ downKeys ++ ": " ++ navigateLabel ++ " | " 
                     ++ quitKeys ++ ": " ++ quitLabel
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
                  msgs = s' ^. i18nMessages
                  env = App.AppEnv conn msgs
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
                  msgs = s' ^. i18nMessages
              liftIO <| App.runAppM (App.AppEnv conn msgs) (App.deleteTodo tid)
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
              msgs = s' ^. i18nMessages
              env = App.AppEnv conn msgs
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
                msgs = s' ^. i18nMessages
                env = App.AppEnv conn msgs
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
tuiMain = tuiMainWithLanguage I18n.Korean

tuiMainWithLanguage :: I18n.Language -> IO ()
tuiMainWithLanguage lang = do
  msgs <- I18n.loadMessages lang
  kb <- Config.loadKeyBindingsWithMessages "config/keybindings.yaml" msgs

  conn <- open "todos.db"
  DB.initDBWithMessages conn msgs

  let env = App.AppEnv conn msgs
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
          , _i18nMessages         = msgs
          }

  void <| defaultMain app initialState
