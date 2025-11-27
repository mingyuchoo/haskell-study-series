{-# LANGUAGE OverloadedStrings #-}

-- | UI rendering functions (Pure)
--
-- This module contains all UI rendering logic.
-- All functions are pure - they take state and return Widget structures
-- without performing any side effects.
--
-- Pure components:
--   - drawUI: Main UI rendering
--   - drawHeader: Header rendering
--   - drawTodoList: Todo list rendering
--   - drawTodo: Individual todo item rendering
--   - drawDetailView: Detail view rendering
--   - drawHelp: Help text rendering
--   - All helper functions
--
-- Effects: NONE - All functions are pure
--
-- Note: While these functions return Widget types (which will eventually
-- be rendered to the terminal), the functions themselves are pure.
-- They only construct data structures describing what to render.
module UI.Draw
    ( drawUI
    ) where

import           Brick                (Widget, attrName, hBox, padAll,
                                       padTopBottom, str, vBox, vLimit,
                                       withAttr)
import qualified Brick
import           Brick.Widgets.Border (borderWithLabel, hBorder)
import           Brick.Widgets.Center (center, hCenter)
import qualified Brick.Widgets.Edit   as E
import           Brick.Widgets.List   (listElementsL, listSelected, renderList)

import qualified Config

import qualified Data.Vector          as Vec

import           Flow                 ((<|))

import qualified I18n

import           Lens.Micro           ((^.))

import           UI.Types

-- | UI 그리기 (Pure)
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

-- | 헤더 그리기 (Pure)
drawHeader :: AppState -> Widget Name
drawHeader s =
  withAttr (attrName "header") $
    hCenter $
      padTopBottom 1 $
        str $
          I18n.header (I18n.ui (s ^. i18nMessages))

-- | Todo 리스트 그리기 (Pure)
drawTodoList :: AppState -> Widget Name
drawTodoList s =
  let msgs = s ^. i18nMessages
      uiMsgs = I18n.ui msgs
   in borderWithLabel (str <| I18n.todos_title uiMsgs) $
        padAll 1 $
          vLimit 20 $
            if null (s ^. todoList . listElementsL)
              then center <| str <| I18n.no_todos uiMsgs
              else renderList (drawTodo msgs) True (s ^. todoList)

-- | 개별 Todo 항목 그리기 (Pure)
drawTodo :: I18n.I18nMessages -> Bool -> Todo -> Widget Name
drawTodo msgs selected todo = withAttr selectAttr todoWidget
  where
    listMsgs = I18n.list msgs
    status = todo ^. todoStatus

    statusIcon =
      str $
        case status of
          "registered"  -> "[R] "
          "in_progress" -> "[P] "
          "cancelled"   -> "[X] "
          "completed"   -> "[✓] "
          _             -> "[ ] "

    todoAttr = attrName <| case status of
      "registered"  -> "registered"
      "in_progress" -> "in_progress"
      "cancelled"   -> "cancelled"
      "completed"   -> "completed"
      _             -> "normal"
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

    fieldMsgs = I18n.fields msgs
    statusChangedText = maybe "" (\t -> I18n.status_changed_label fieldMsgs <> ": " <> t <> I18n.field_separator listMsgs) (todo ^. todoStatusChangedAt)

    timestampText = statusChangedText <> I18n.created_prefix listMsgs <> todo ^. todoCreatedAt
    timestampWidth = stringWidth timestampText + 2  -- 여백 포함

    -- 할일 내용과 타임스탬프를 동적으로 배치
    todoWidget = Brick.Widget Brick.Greedy Brick.Fixed $ do
      ctx <- Brick.getContext
      let totalWidth = Brick.availWidth ctx
          statusIconWidth = 4  -- "[R] " 등의 너비
          availableForMain = totalWidth - statusIconWidth - timestampWidth
          truncatedMain = truncateWithEllipsis availableForMain mainInfo
          -- 할일 내용 뒤에 공백을 채워서 타임스탬프가 오른쪽 끝에 오도록 함
          paddingWidth = max 0 (availableForMain - stringWidth truncatedMain)
          padding = replicate paddingWidth ' '
      Brick.render $ hBox
        [ statusIcon
        , str truncatedMain
        , str padding
        , withAttr (attrName "timestamp") $ str timestampText
        ]

-- | 문자열의 터미널 표시 너비 계산 (한글은 2칸, ASCII는 1칸)
stringWidth :: String -> Int
stringWidth = sum . map charWidth
  where
    charWidth c
      | c >= '\x1100' && c <= '\x11FF'   = 2  -- 한글 자모
      | c >= '\x3000' && c <= '\x303F'   = 2  -- CJK 구두점
      | c >= '\x3130' && c <= '\x318F'   = 2  -- 한글 호환 자모
      | c >= '\xAC00' && c <= '\xD7AF'   = 2  -- 한글 음절
      | c >= '\xFF00' && c <= '\xFFEF'   = 2  -- 전각 문자
      | c >= '\x4E00' && c <= '\x9FFF'   = 2  -- CJK 통합 한자
      | otherwise                        = 1

-- | 문자열을 지정된 터미널 너비로 자르고 줄임표 추가 (Pure)
truncateWithEllipsis :: Int -> String -> String
truncateWithEllipsis maxWidth text
  | stringWidth text <= maxWidth = text
  | maxWidth <= 3                = "..."
  | otherwise                    = truncateToWidth (maxWidth - 3) text <> "..."

-- | 문자열을 지정된 터미널 너비까지 자르기
truncateToWidth :: Int -> String -> String
truncateToWidth maxWidth = go 0
  where
    go _ [] = []
    go currentWidth (c:cs)
      | currentWidth + charWidth c > maxWidth = []
      | otherwise = c : go (currentWidth + charWidth c) cs
    charWidth c
      | c >= '\x1100' && c <= '\x11FF'   = 2
      | c >= '\x3000' && c <= '\x303F'   = 2
      | c >= '\x3130' && c <= '\x318F'   = 2
      | c >= '\xAC00' && c <= '\xD7AF'   = 2
      | c >= '\xFF00' && c <= '\xFFEF'   = 2
      | c >= '\x4E00' && c <= '\x9FFF'   = 2
      | otherwise                        = 1

-- | 상세 뷰 그리기 (Pure)
drawDetailView :: AppState -> Widget Name
drawDetailView s =
  let msgs = s ^. i18nMessages
      uiMsgs = I18n.ui msgs
   in case s ^. mode of
        ViewMode   -> drawViewModeDetail s msgs uiMsgs
        EditMode _ -> drawEditModeDetail s msgs uiMsgs
        InputMode  -> drawInputModeDetail s msgs uiMsgs

-- | ViewMode 상세 뷰 (Pure)
drawViewModeDetail :: AppState -> I18n.I18nMessages -> I18n.UIMessages -> Widget Name
drawViewModeDetail s msgs uiMsgs =
  case listSelected (s ^. todoList) of
    Nothing -> emptyDetailView uiMsgs (I18n.no_selection uiMsgs)
    Just idx ->
      let todos = s ^. todoList . listElementsL
       in case todos Vec.!? idx of
            Nothing   -> emptyDetailView uiMsgs (I18n.no_selection uiMsgs)
            Just todo -> drawTodoDetail msgs uiMsgs todo False

-- | EditMode 상세 뷰 (Pure)
drawEditModeDetail :: AppState -> I18n.I18nMessages -> I18n.UIMessages -> Widget Name
drawEditModeDetail s msgs uiMsgs =
  case s ^. editingIndex of
    Nothing -> emptyDetailView uiMsgs (I18n.not_found uiMsgs)
    Just idx ->
      let todos = s ^. todoList . listElementsL
       in case todos Vec.!? idx of
            Nothing   -> emptyDetailView uiMsgs (I18n.not_found uiMsgs)
            Just todo -> drawTodoDetailWithEditors s msgs uiMsgs todo (I18n.detail_edit_title uiMsgs)

-- | InputMode 상세 뷰 (Pure)
drawInputModeDetail :: AppState -> I18n.I18nMessages -> I18n.UIMessages -> Widget Name
drawInputModeDetail s msgs uiMsgs =
  let fieldMsgs = I18n.fields msgs
      statusMsgs = I18n.status msgs
   in borderWithLabel (str <| I18n.detail_add_title uiMsgs) $
        padAll 1 $
          vLimit 8 $
            vBox
              [ hBox
                  [ withAttr (attrName "detailLabel") <| str (I18n.id_label fieldMsgs <> ": "),
                    withAttr (attrName "timestamp") <| str <| I18n.auto_generated_label fieldMsgs
                  ],
                hBox
                  [ withAttr (attrName "detailLabel") <| str (I18n.status_label fieldMsgs <> ": "),
                    str <| I18n.in_progress statusMsgs
                  ],
                renderEditField s fieldMsgs (I18n.action_required_label fieldMsgs) (s ^. actionEditor) FocusAction,
                renderEditField s fieldMsgs (I18n.subject_label fieldMsgs) (s ^. subjectEditor) FocusSubject,
                renderEditField s fieldMsgs (I18n.indirect_object_label fieldMsgs) (s ^. indirectObjectEditor) FocusIndirectObject,
                renderEditField s fieldMsgs (I18n.direct_object_label fieldMsgs) (s ^. directObjectEditor) FocusDirectObject,
                hBox
                  [ withAttr (attrName "detailLabel") <| str (I18n.created_at_label fieldMsgs <> ": "),
                    withAttr (attrName "timestamp") <| str <| I18n.auto_generated_label fieldMsgs
                  ],
                str ""
              ]

-- | 빈 상세 뷰 (Pure)
emptyDetailView :: I18n.UIMessages -> String -> Widget Name
emptyDetailView uiMsgs msg =
  borderWithLabel (str <| I18n.detail_title uiMsgs) $
    padAll 1 $
      center $
        str msg

-- | Todo 상세 정보 그리기 (Pure, 읽기 전용)
drawTodoDetail :: I18n.I18nMessages -> I18n.UIMessages -> Todo -> Bool -> Widget Name
drawTodoDetail msgs uiMsgs todo _ =
  let fieldMsgs = I18n.fields msgs
      statusMsgs = I18n.status msgs
      status = todo ^. todoStatus
      statusText = case status of
        "registered"  -> "Registered"
        "in_progress" -> I18n.in_progress statusMsgs
        "cancelled"   -> "Cancelled"
        "completed"   -> I18n.completed statusMsgs
        _             -> "Unknown"
      statusAttr = attrName <| case status of
        "registered"  -> "registered"
        "in_progress" -> "in_progress"
        "cancelled"   -> "cancelled"
        "completed"   -> "completed"
        _             -> "normal"

      showDetailField _ Nothing = str ""
      showDetailField lbl (Just val) =
        hBox
          [ withAttr (attrName "detailLabel") <| str (lbl <> ": "),
            str val
          ]

      statusChangedInfo = case todo ^. todoStatusChangedAt of
        Just changeTime ->
          hBox
            [ withAttr (attrName "detailLabel") <| str (I18n.status_changed_label fieldMsgs <> ": "),
              withAttr (attrName "timestamp") <| str changeTime
            ]
        Nothing -> str ""
   in borderWithLabel (str <| I18n.detail_title uiMsgs) $
        padAll 1 $
          vLimit 8 $
            vBox
              [ hBox
                  [ withAttr (attrName "detailLabel") <| str (I18n.id_label fieldMsgs <> ": "),
                    str (show (todo ^. todoId))
                  ],
                hBox
                  [ withAttr (attrName "detailLabel") <| str (I18n.status_label fieldMsgs <> ": "),
                    withAttr statusAttr <| str statusText
                  ],
                hBox
                  [ withAttr (attrName "detailLabel") <| str (I18n.action_label fieldMsgs <> ": "),
                    str (todo ^. todoAction)
                  ],
                showDetailField (I18n.subject_label fieldMsgs) (todo ^. todoSubject),
                showDetailField (I18n.indirect_object_label fieldMsgs) (todo ^. todoIndirectObject),
                showDetailField (I18n.direct_object_label fieldMsgs) (todo ^. todoDirectObject),
                hBox
                  [ withAttr (attrName "detailLabel") <| str (I18n.created_at_label fieldMsgs <> ": "),
                    withAttr (attrName "timestamp") <| str (todo ^. todoCreatedAt)
                  ],
                statusChangedInfo
              ]

-- | Todo 상세 정보 그리기 (Pure, 편집 가능)
drawTodoDetailWithEditors :: AppState -> I18n.I18nMessages -> I18n.UIMessages -> Todo -> String -> Widget Name
drawTodoDetailWithEditors s msgs _uiMsgs todo title =
  let fieldMsgs = I18n.fields msgs
      statusMsgs = I18n.status msgs
      status = todo ^. todoStatus
      statusText = case status of
        "registered"  -> "Registered"
        "in_progress" -> I18n.in_progress statusMsgs
        "cancelled"   -> "Cancelled"
        "completed"   -> I18n.completed statusMsgs
        _             -> "Unknown"
      statusAttr = attrName <| case status of
        "registered"  -> "registered"
        "in_progress" -> "in_progress"
        "cancelled"   -> "cancelled"
        "completed"   -> "completed"
        _             -> "normal"

      statusChangedInfo = case todo ^. todoStatusChangedAt of
        Just changeTime ->
          hBox
            [ withAttr (attrName "detailLabel") <| str (I18n.status_changed_label fieldMsgs <> ": "),
              withAttr (attrName "timestamp") <| str changeTime
            ]
        Nothing -> str ""
   in borderWithLabel (str title) $
        padAll 1 $
          vLimit 8 $
            vBox
              [ hBox
                  [ withAttr (attrName "detailLabel") <| str (I18n.id_label fieldMsgs <> ": "),
                    str (show (todo ^. todoId))
                  ],
                hBox
                  [ withAttr (attrName "detailLabel") <| str (I18n.status_label fieldMsgs <> ": "),
                    withAttr statusAttr <| str statusText
                  ],
                renderEditField s fieldMsgs (I18n.action_required_label fieldMsgs) (s ^. actionEditor) FocusAction,
                renderEditField s fieldMsgs (I18n.subject_label fieldMsgs) (s ^. subjectEditor) FocusSubject,
                renderEditField s fieldMsgs (I18n.indirect_object_label fieldMsgs) (s ^. indirectObjectEditor) FocusIndirectObject,
                renderEditField s fieldMsgs (I18n.direct_object_label fieldMsgs) (s ^. directObjectEditor) FocusDirectObject,
                hBox
                  [ withAttr (attrName "detailLabel") <| str (I18n.created_at_label fieldMsgs <> ": "),
                    withAttr (attrName "timestamp") <| str (todo ^. todoCreatedAt)
                  ],
                statusChangedInfo
              ]

-- | 편집 가능한 필드 렌더링 (Pure)
renderEditField :: AppState -> I18n.FieldLabels -> String -> E.Editor String Name -> FocusedField -> Widget Name
renderEditField s _ fieldLabel editor fieldType =
  let isFocused = s ^. focusedField == fieldType
      fieldAttr = if isFocused then attrName "focusedField" else attrName "detailLabel"
      labelWidget = withAttr fieldAttr <| str (fieldLabel <> ": ")
      editorWidget = E.renderEditor (str . unlines) isFocused editor
   in hBox [labelWidget, editorWidget]

-- | 도움말 그리기 (Pure)
drawHelp :: AppState -> Widget Name
drawHelp s =
  let msgs = s ^. i18nMessages
      helpMsgs = I18n.help msgs
   in padAll 1 $
        case s ^. mode of
          InputMode  -> str <| I18n.input_mode helpMsgs
          EditMode _ -> str <| I18n.edit_mode helpMsgs
          ViewMode   -> drawViewModeHelp s helpMsgs

-- | ViewMode 도움말 (Pure)
drawViewModeHelp :: AppState -> I18n.HelpMessages -> Widget Name
drawViewModeHelp s helpMsgs =
  let kb = s ^. keyBindings
      quitKeys = Config.getFirstKey (Config.quit kb) "q"
      addKeys = Config.getFirstKey (Config.add_todo kb) "a"
      editKeys = Config.getFirstKey (Config.edit_todo kb) "e"
      toggleKeys = Config.getFirstKey (Config.toggle_complete kb) "t"
      deleteKeys = Config.getFirstKey (Config.delete_todo kb) "d"
      upKeys = Config.getFirstKey (Config.navigate_up kb) "k"
      downKeys = Config.getFirstKey (Config.navigate_down kb) "j"
   in vBox
        [ str $
            addKeys
              <> ": "
              <> I18n.add helpMsgs
              <> " | "
              <> editKeys
              <> ": "
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
