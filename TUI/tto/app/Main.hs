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

import qualified TodoService

projectName :: String
projectName = "tto"

getConfigDir :: IO FilePath
getConfigDir = do
    homeDir <- getHomeDirectory
    let configDir = homeDir </> ".config" </> projectName
    createDirectoryIfMissing True configDir
    return configDir

getDBPath :: IO FilePath
getDBPath = do
    configDir <- getConfigDir
    return <| configDir </> "todos.db"

getKeyBindingsPath :: IO FilePath
getKeyBindingsPath = do
    configDir <- getConfigDir
    return <| configDir </> "keybindings.yaml"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    appWithLanguage I18n.Korean

appWithLanguage :: I18n.Language -> IO ()
appWithLanguage lang = do
    msgs <- I18n.loadMessages lang
    kbPath <- getKeyBindingsPath
    kb <- Config.loadKeyBindingsWithMessages kbPath msgs

    dbPath <- getDBPath
    conn <- open dbPath
    DB.initDBWithMessages conn msgs

    -- Create AppEnv with all dependencies
    let env = App.AppEnv
            { App.envConnection = conn
            , App.envMessages = msgs
            , App.envKeyBindings = kb
            }

    -- Load todos using Tagless Final
    todoRows <- App.runAppM env TodoService.loadAllTodos

    let initialTodos = Vec.fromList <| map fromTodoRow todoRows
        initialState = AppState
            { _todoList = list TodoList initialTodos 1
            , _actionEditor = E.editor ActionField (Just 1) ""
            , _subjectEditor = E.editor SubjectField (Just 1) ""
            , _indirectObjectEditor = E.editor IndirectObjectField (Just 1) ""
            , _directObjectEditor = E.editor DirectObjectField (Just 1) ""
            , _focusedField = FocusAction
            , _mode = ViewMode
            , _appEnv = env
            , _editingIndex = Nothing
            , _i18nMessages = msgs
            }

    void <| defaultMain app initialState
