{-# LANGUAGE OverloadedStrings #-}

-- | Application entry point and integration (MIXED: Pure + Effectful)
--
-- This module integrates all components and defines the Brick application.
--
-- Pure components:
--   - app: Brick application definition (structure)
--   - Re-exports from UI.Types
--
-- Effectful components:
--   - appHandleEvent: References UI.Events.handleEvent (effectful)
--
-- Note: While the app definition itself is pure data, it references
-- effectful event handlers, making the overall application effectful.
module Lib
    ( AppState (..)
    , FocusedField (..)
    , Mode (..)
    , Name (..)
    , Todo (..)
    , actionEditor
    , app
    , directObjectEditor
    , editingIndex
    , focusedField
    , fromTodoRow
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
    ) where

import           Brick         (App (..), showCursorNamed)

import           Lens.Micro    ((^.))

import           UI.Attributes (theMap)
import           UI.Draw       (drawUI)
import           UI.Events     (handleEvent, trim)
import           UI.Types

-- | 애플리케이션 정의 (Pure)
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
        ViewMode -> Nothing,
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap = const theMap
    }
