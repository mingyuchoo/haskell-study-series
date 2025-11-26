module Main
    ( main
    ) where

import           Lib

import           System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

-- |
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  appWithLanguage I18n.Korean

-- |
appWithLanguage :: I18n.Language -> IO ()
appWithLanguage lang = do
  msgs <- I18n.loadMessages lang
  kb <- Config.loadKeyBindingsWithMessages "config/keybindings.yaml" msgs

  conn <- open "todos.db"
  DB.initDBWithMessages conn msgs

  let env = App.AppEnv conn msgs
  todoRows <- App.runAppM env App.loadTodos

  let initialTodos = Vec.fromList <| map fromTodoRow todoRows
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
            _editingIndex = Nothing,
            _i18nMessages = msgs
          }

  void <| defaultMain app initialState
