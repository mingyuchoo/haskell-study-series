module Main
    ( main
    ) where

import qualified App

import           Brick                  (defaultMain)
import qualified Brick.Widgets.Edit     as E
import           Brick.Widgets.List     (list)

import qualified Config

import           Control.Monad          (void)

import qualified DB

import qualified Data.Vector            as Vec

import           Database.SQLite.Simple (open)

import           Flow                   ((<|))

import qualified I18n

import           Lib

import           System.Directory       (createDirectoryIfMissing,
                                         getHomeDirectory)
import           System.FilePath        ((</>))
import           System.IO              (BufferMode (NoBuffering),
                                         hSetBuffering, stdout)

-- | Project name for config directory
projectName :: String
projectName = "tui-todo"

-- | Get the config directory path in $HOME/.config/tui-todo/
getConfigDir :: IO FilePath
getConfigDir = do
  homeDir <- getHomeDirectory
  let configDir = homeDir </> ".config" </> projectName
  createDirectoryIfMissing True configDir
  return configDir

-- | Get the database file path in $HOME/.config/tui-todo/
getDBPath :: IO FilePath
getDBPath = do
  configDir <- getConfigDir
  return <| configDir </> "todos.db"

-- | Get the keybindings config file path in $HOME/.config/tui-todo/
getKeyBindingsPath :: IO FilePath
getKeyBindingsPath = do
  configDir <- getConfigDir
  return <| configDir </> "keybindings.yaml"

-- |
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  appWithLanguage I18n.Korean

-- |
appWithLanguage :: I18n.Language -> IO ()
appWithLanguage lang = do
  msgs <- I18n.loadMessages lang
  kbPath <- getKeyBindingsPath
  kb <- Config.loadKeyBindingsWithMessages kbPath msgs

  dbPath <- getDBPath
  conn <- open dbPath
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
