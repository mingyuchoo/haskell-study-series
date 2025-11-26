{-# LANGUAGE OverloadedStrings #-}

module UI.Draw
    ( drawUI
    ) where

import           Brick                (Padding (..), Widget, attrName, hBox,
                                       padAll, padLeft, padTopBottom, str,
                                       vBox, vLimit, withAttr)
import           Brick.Widgets.Border (borderWithLabel, hBorder)
import           Brick.Widgets.Center (center, hCenter)
import qualified Brick.Widgets.Edit   as E
import           Brick.Widgets.List   (listElementsL, listSelected, renderList)

import qualified Config
import qualified Data.Vector          as Vec

import qualified I18n
import           Lens.Micro           ((^.))

import           Types

-- | UI 그리기
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

-- | 헤더 그리기
drawHeader :: AppState -> Widget Name
drawHeader s =
  withAttr (attrName "header") $
    hCenter $
      padTopBottom 1 $
        str $
          I18n.header (I18n.ui (s ^. i18nMessages))

-- | Todo 리스트 그리기
drawTodoList :: AppState -> Widget Name
drawTodoList s =
  let msgs = s ^. i18nMessages
      uiMsgs = I18n.ui msgs
   in borderWithLabel (str $ I18n.todos_title uiMsgs) $
        padAll 1 $
          vLimit 20 $
            if null (s ^. todoList . listElementsL)
              then center $ str $ I18n.no_todos uiMsgs
              else renderList (drawTodo msgs) True (s ^. todoList)

-- | 개별 Todo 항목 그리기
drawTodo :: I18n.I18nMessages -> Bool -> Todo -> Widget Name
drawTodo msgs selected todo = withAttr selectAttr $ hBox [checkbox, str mainInfo, timestamp]
  where
    listMsgs = I18n.list msgs

    checkbox =
      str $
        if todo ^. todoCompleted
          then I18n.checkbox_done listMsgs
          else I18n.checkbox_todo listMsgs

    todoAttr = attrName $ if todo ^. todoCompleted then "completed" else "normal"
    selectAttr = if selected then attrName "selected" else todoAttr

    showField _ Nothing    = ""
    showField lbl (Just v) = I18n.field_separator listMsgs <> lbl <> ": " <> v

    mainInfo =
      concat
        [ I18n.field_action listMsgs <> ": " <> todo ^. todoAction,
          showField (I18n.field_subject listMsgs) (todo ^. todoSubject),
          showField (I18n.field_indirect listMsgs) (todo ^. todoIndirectObject),
          showField (I18n.field_direct listMsgs) (todo ^. todoDirectObject)
        ]

    completedTimeText = maybe "" (\t -> I18n.completed_prefix listMsgs <> t <> I18n.field_separator listMsgs) (todo ^. todoCompletedAt)

    timestamp =
      padLeft Max $
        withAttr (attrName "timestamp") $
          str (completedTimeText <> I18n.created_prefix listMsgs <> todo ^. todoCreatedAt)

-- | 상세 뷰 그리기
drawDetailView :: AppState -> Widget Name
drawDetailView s =
  let msgs = s ^. i18nMessages
      uiMsgs = I18n.ui msgs
   in case s ^. mode of
        ViewMode   -> drawViewModeDetail s msgs uiMsgs
        EditMode _ -> drawEditModeDetail s msgs uiMsgs
        InputMode  -> drawInputModeDetail s msgs uiMsgs

-- | ViewMode 상세 뷰
drawViewModeDetail :: AppState -> I18n.I18nMessages -> I18n.UIMessages -> Widget Name
drawViewModeDetail s msgs uiMsgs =
  case listSelected (s ^. todoList) of
    Nothing -> emptyDetailView uiMsgs (I18n.no_selection uiMsgs)
    Just idx ->
      let todos = s ^. todoList . listElementsL
       in case todos Vec.!? idx of
            Nothing   -> emptyDetailView uiMsgs (I18n.no_selection uiMsgs)
            Just todo -> drawTodoDetail msgs uiMsgs todo False

-- | EditMode 상세 뷰
drawEditModeDetail :: AppState -> I18n.I18nMessages -> I18n.UIMessages -> Widget Name
drawEditModeDetail s msgs uiMsgs =
  case s ^. editingIndex of
    Nothing -> emptyDetailView uiMsgs (I18n.not_found uiMsgs)
    Just idx ->
      let todos = s ^. todoList . listElementsL
       in case todos Vec.!? idx of
            Nothing   -> emptyDetailView uiMsgs (I18n.not_found uiMsgs)
            Just todo -> drawTodoDetailWithEditors s msgs uiMsgs todo (I18n.detail_edit_title uiMsgs)

-- | InputMode 상세 뷰
drawInputModeDetail :: AppState -> I18n.I18nMessages -> I18n.UIMessages -> Widget Name
drawInputModeDetail s msgs uiMsgs =
  let fieldMsgs = I18n.fields msgs
      statusMsgs = I18n.status msgs
   in borderWithLabel (str $ I18n.detail_add_title uiMsgs) $
        padAll 1 $
          vLimit 8 $
            vBox
              [ hBox
                  [ withAttr (attrName "detailLabel") $ str (I18n.id_label fieldMsgs <> ": "),
                    withAttr (attrName "timestamp") $ str $ I18n.auto_generated_label fieldMsgs
                  ],
                hBox
                  [ withAttr (attrName "detailLabel") $ str (I18n.status_label fieldMsgs <> ": "),
                    str $ I18n.in_progress statusMsgs
                  ],
                renderEditField s fieldMsgs (I18n.action_required_label fieldMsgs) (s ^. actionEditor) FocusAction,
                renderEditField s fieldMsgs (I18n.subject_label fieldMsgs) (s ^. subjectEditor) FocusSubject,
                renderEditField s fieldMsgs (I18n.indirect_object_label fieldMsgs) (s ^. indirectObjectEditor) FocusIndirectObject,
                renderEditField s fieldMsgs (I18n.direct_object_label fieldMsgs) (s ^. directObjectEditor) FocusDirectObject,
                hBox
                  [ withAttr (attrName "detailLabel") $ str (I18n.created_at_label fieldMsgs <> ": "),
                    withAttr (attrName "timestamp") $ str $ I18n.auto_generated_label fieldMsgs
                  ],
                str ""
              ]

-- | 빈 상세 뷰
emptyDetailView :: I18n.UIMessages -> String -> Widget Name
emptyDetailView uiMsgs msg =
  borderWithLabel (str $ I18n.detail_title uiMsgs) $
    padAll 1 $
      center $
        str msg

-- | Todo 상세 정보 그리기 (읽기 전용)
drawTodoDetail :: I18n.I18nMessages -> I18n.UIMessages -> Todo -> Bool -> Widget Name
drawTodoDetail msgs uiMsgs todo _ =
  let fieldMsgs = I18n.fields msgs
      statusMsgs = I18n.status msgs
      statusText = if todo ^. todoCompleted then I18n.completed statusMsgs else I18n.in_progress statusMsgs
      statusAttr = if todo ^. todoCompleted then attrName "completed" else attrName "normal"

      showDetailField _ Nothing = str ""
      showDetailField lbl (Just val) =
        hBox
          [ withAttr (attrName "detailLabel") $ str (lbl <> ": "),
            str val
          ]

      completedInfo = case todo ^. todoCompletedAt of
        Just compTime ->
          hBox
            [ withAttr (attrName "detailLabel") $ str (I18n.completed_at_label fieldMsgs <> ": "),
              withAttr (attrName "timestamp") $ str compTime
            ]
        Nothing -> str ""
   in borderWithLabel (str $ I18n.detail_title uiMsgs) $
        padAll 1 $
          vLimit 8 $
            vBox
              [ hBox
                  [ withAttr (attrName "detailLabel") $ str (I18n.id_label fieldMsgs <> ": "),
                    str (show (todo ^. todoId))
                  ],
                hBox
                  [ withAttr (attrName "detailLabel") $ str (I18n.status_label fieldMsgs <> ": "),
                    withAttr statusAttr $ str statusText
                  ],
                hBox
                  [ withAttr (attrName "detailLabel") $ str (I18n.action_label fieldMsgs <> ": "),
                    str (todo ^. todoAction)
                  ],
                showDetailField (I18n.subject_label fieldMsgs) (todo ^. todoSubject),
                showDetailField (I18n.indirect_object_label fieldMsgs) (todo ^. todoIndirectObject),
                showDetailField (I18n.direct_object_label fieldMsgs) (todo ^. todoDirectObject),
                hBox
                  [ withAttr (attrName "detailLabel") $ str (I18n.created_at_label fieldMsgs <> ": "),
                    withAttr (attrName "timestamp") $ str (todo ^. todoCreatedAt)
                  ],
                completedInfo
              ]

-- | Todo 상세 정보 그리기 (편집 가능)
drawTodoDetailWithEditors :: AppState -> I18n.I18nMessages -> I18n.UIMessages -> Todo -> String -> Widget Name
drawTodoDetailWithEditors s msgs _uiMsgs todo title =
  let fieldMsgs = I18n.fields msgs
      statusMsgs = I18n.status msgs
      statusText = if todo ^. todoCompleted then I18n.completed statusMsgs else I18n.in_progress statusMsgs
      statusAttr = if todo ^. todoCompleted then attrName "completed" else attrName "normal"

      completedInfo = case todo ^. todoCompletedAt of
        Just compTime ->
          hBox
            [ withAttr (attrName "detailLabel") $ str (I18n.completed_at_label fieldMsgs <> ": "),
              withAttr (attrName "timestamp") $ str compTime
            ]
        Nothing -> str ""
   in borderWithLabel (str title) $
        padAll 1 $
          vLimit 8 $
            vBox
              [ hBox
                  [ withAttr (attrName "detailLabel") $ str (I18n.id_label fieldMsgs <> ": "),
                    str (show (todo ^. todoId))
                  ],
                hBox
                  [ withAttr (attrName "detailLabel") $ str (I18n.status_label fieldMsgs <> ": "),
                    withAttr statusAttr $ str statusText
                  ],
                renderEditField s fieldMsgs (I18n.action_required_label fieldMsgs) (s ^. actionEditor) FocusAction,
                renderEditField s fieldMsgs (I18n.subject_label fieldMsgs) (s ^. subjectEditor) FocusSubject,
                renderEditField s fieldMsgs (I18n.indirect_object_label fieldMsgs) (s ^. indirectObjectEditor) FocusIndirectObject,
                renderEditField s fieldMsgs (I18n.direct_object_label fieldMsgs) (s ^. directObjectEditor) FocusDirectObject,
                hBox
                  [ withAttr (attrName "detailLabel") $ str (I18n.created_at_label fieldMsgs <> ": "),
                    withAttr (attrName "timestamp") $ str (todo ^. todoCreatedAt)
                  ],
                completedInfo
              ]

-- | 편집 가능한 필드 렌더링
renderEditField :: AppState -> I18n.FieldLabels -> String -> E.Editor String Name -> FocusedField -> Widget Name
renderEditField s _ fieldLabel editor fieldType =
  let isFocused = s ^. focusedField == fieldType
      fieldAttr = if isFocused then attrName "focusedField" else attrName "detailLabel"
      labelWidget = withAttr fieldAttr $ str (fieldLabel <> ": ")
      editorWidget = E.renderEditor (str . unlines) isFocused editor
   in hBox [labelWidget, editorWidget]

-- | 도움말 그리기
drawHelp :: AppState -> Widget Name
drawHelp s =
  let msgs = s ^. i18nMessages
      helpMsgs = I18n.help msgs
   in padAll 1 $
        case s ^. mode of
          InputMode  -> str $ I18n.input_mode helpMsgs
          EditMode _ -> str $ I18n.edit_mode helpMsgs
          ViewMode   -> drawViewModeHelp s helpMsgs

-- | ViewMode 도움말
drawViewModeHelp :: AppState -> I18n.HelpMessages -> Widget Name
drawViewModeHelp s helpMsgs =
  let kb = s ^. keyBindings
      quitKeys = Config.getFirstKey (Config.quit kb) "q"
      addKeys = Config.getFirstKey (Config.add_todo kb) "a"
      toggleKeys = Config.getFirstKey (Config.toggle_complete kb) "t"
      deleteKeys = Config.getFirstKey (Config.delete_todo kb) "d"
      upKeys = Config.getFirstKey (Config.navigate_up kb) "k"
      downKeys = Config.getFirstKey (Config.navigate_down kb) "j"
   in vBox
        [ str $
            addKeys
              <> ": "
              <> I18n.add helpMsgs
              <> " | e: "
              <> I18n.edit helpMsgs
              <> " | "
              <> toggleKeys
              <> ": "
              <> I18n.toggle helpMsgs
              <> " | "
              <> deleteKeys
              <> ": "
              <> I18n.delete helpMsgs
              <> " | "
              <> upKeys
              <> "/"
              <> downKeys
              <> ": "
              <> I18n.navigate helpMsgs
              <> " | "
              <> quitKeys
              <> ": "
              <> I18n.quit helpMsgs
        ]
