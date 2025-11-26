{-# LANGUAGE TemplateHaskell #-}

module Types
    ( AppState (..)
    , FocusedField (..)
    , Mode (..)
    , Name (..)
    , Todo (..)
    , actionEditor
    , dbConn
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
    ) where

import qualified Brick.Widgets.Edit as E
import           Brick.Widgets.List (List)

import qualified Config
import qualified DB
import           Database.SQLite.Simple (Connection)
import qualified I18n

import           Lens.Micro.TH (makeLenses)

-- | Application modes
data Mode = ViewMode
          | InputMode
          | EditMode DB.TodoId
     deriving (Eq, Show)

-- | Widget resource names
data Name = TodoList | ActionField | SubjectField | IndirectObjectField | DirectObjectField
     deriving (Eq, Ord, Show)

-- | Field focus tracking
data FocusedField = FocusAction | FocusSubject | FocusIndirectObject | FocusDirectObject
     deriving (Eq, Show)

-- | UI representation of a Todo item
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

-- | Application state
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

-- | Convert DB TodoRow to UI Todo
fromTodoRow :: DB.TodoRow -> Todo
fromTodoRow row =
  Todo
    { _todoId = DB.todoId row,
      _todoAction = DB.todoAction row,
      _todoCompleted = DB.todoCompleted row,
      _todoCreatedAt = DB.todoCreatedAt row,
      _todoSubject = DB.todoSubject row,
      _todoObject = DB.todoObject row,
      _todoIndirectObject = DB.todoIndirectObject row,
      _todoDirectObject = DB.todoDirectObject row,
      _todoCompletedAt = DB.todoCompletedAt row
    }
