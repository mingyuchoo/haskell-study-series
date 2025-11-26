{-# LANGUAGE OverloadedStrings #-}

module DB
    ( TodoId
    , TodoRow (..)
    , createTodo
    , createTodoWithFields
    , deleteTodo
    , getAllTodos
    , initDB
    , initDBWithMessages
    , toggleTodoComplete
    , updateTodo
    , updateTodoWithFields
    ) where

import           Data.Time.Clock        (getCurrentTime)
import           Data.Time.Format       (defaultTimeLocale, formatTime)

import           Database.SQLite.Simple (Connection, FromRow (..), Only (..),
                                         ToRow (..), execute, execute_, field,
                                         lastInsertRowId, query_)

import qualified I18n

type TodoId = Int

-- | Domain model for a Todo item
data TodoRow = TodoRow { todoId             :: !TodoId
                       , todoAction         :: !String
                       , todoCompleted      :: !Bool
                       , todoCreatedAt      :: !String
                       , todoSubject        :: !(Maybe String)
                       , todoObject         :: !(Maybe String)
                       , todoIndirectObject :: !(Maybe String)
                       , todoDirectObject   :: !(Maybe String)
                       , todoCompletedAt    :: !(Maybe String)
                       }
     deriving (Eq, Show)

instance FromRow TodoRow where
    fromRow = TodoRow
        <$> field <*> field <*> field <*> field
        <*> field <*> field <*> field <*> field <*> field

instance ToRow TodoRow where
    toRow (TodoRow tid txt comp created subj obj indObj dirObj compAt) =
        toRow (tid, txt, comp, created, subj, obj, indObj, dirObj, compAt)

-- | Initialize database schema and seed data
initDB :: Connection -> IO ()
initDB conn = initDBWithMessages conn I18n.defaultMessages

-- | Initialize database with custom messages
initDBWithMessages :: Connection -> I18n.I18nMessages -> IO ()
initDBWithMessages conn msgs = do
    createTodosTable
    seedInitialData
  where
    createTodosTable = execute_ conn
        "CREATE TABLE IF NOT EXISTS todos \
        \(id INTEGER PRIMARY KEY AUTOINCREMENT, \
        \ text TEXT NOT NULL, \
        \ completed INTEGER NOT NULL DEFAULT 0, \
        \ created_at TEXT NOT NULL, \
        \ subject TEXT, \
        \ object TEXT, \
        \ indirect_object TEXT, \
        \ direct_object TEXT, \
        \ completed_at TEXT)"

    seedInitialData = do
        count <- query_ conn "SELECT COUNT(*) FROM todos" :: IO [Only Int]
        case count of
            [Only 0] -> insertSampleTodos
            _        -> pure ()

    insertSampleTodos = do
        timestamp <- getCurrentTime
        let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" timestamp
            insertTodo txt completed = execute conn
                "INSERT INTO todos (text, completed, created_at, subject, object, \
                \indirect_object, direct_object, completed_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
                (txt :: String, completed, timeStr,
                 Nothing :: Maybe String, Nothing :: Maybe String,
                 Nothing :: Maybe String, Nothing :: Maybe String,
                 if completed then Just timeStr else Nothing :: Maybe String)
            samples = I18n.sample_todos msgs

        insertTodo (I18n.welcome samples) False
        insertTodo (I18n.add_hint samples) False
        insertTodo (I18n.toggle_hint samples) True

-- | Create a new todo with action text only
createTodo :: Connection -> String -> IO TodoId
createTodo conn text = do
    timeStr <- formatCurrentTime
    execute conn
        "INSERT INTO todos (text, completed, created_at, subject, object, \
        \indirect_object, direct_object, completed_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        (text, False, timeStr,
         Nothing :: Maybe String, Nothing :: Maybe String,
         Nothing :: Maybe String, Nothing :: Maybe String,
         Nothing :: Maybe String)
    fromIntegral <$> lastInsertRowId conn

-- | Create a new todo with all fields
createTodoWithFields :: Connection
                     -> String
                     -> Maybe String
                     -> Maybe String
                     -> Maybe String
                     -> IO TodoId
createTodoWithFields conn text subj indObj dirObj = do
    timeStr <- formatCurrentTime
    execute conn
        "INSERT INTO todos (text, completed, created_at, subject, object, \
        \indirect_object, direct_object, completed_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        (text, False, timeStr, subj, Nothing :: Maybe String, indObj, dirObj,
         Nothing :: Maybe String)
    fromIntegral <$> lastInsertRowId conn

-- | Retrieve all todos ordered by ID descending
getAllTodos :: Connection -> IO [TodoRow]
getAllTodos conn = query_ conn
    "SELECT id, text, completed, created_at, subject, object, \
    \indirect_object, direct_object, completed_at \
    \FROM todos ORDER BY id DESC"

-- | Update all fields of a todo
updateTodo :: Connection
           -> TodoId
           -> String
           -> Bool
           -> Maybe String
           -> Maybe String
           -> Maybe String
           -> Maybe String
           -> Maybe String
           -> IO ()
updateTodo conn tid text completed subj obj indObj dirObj compAt =
    execute conn
        "UPDATE todos SET text = ?, completed = ?, subject = ?, object = ?, \
        \indirect_object = ?, direct_object = ?, completed_at = ? WHERE id = ?"
        (text, completed, subj, obj, indObj, dirObj, compAt, tid)

-- | Update specific fields of a todo
updateTodoWithFields :: Connection
                     -> TodoId
                     -> String
                     -> Maybe String
                     -> Maybe String
                     -> Maybe String
                     -> IO ()
updateTodoWithFields conn tid text subj indObj dirObj =
    execute conn
        "UPDATE todos SET text = ?, subject = ?, indirect_object = ?, \
        \direct_object = ? WHERE id = ?"
        (text, subj, indObj, dirObj, tid)

-- | Delete a todo by ID
deleteTodo :: Connection -> TodoId -> IO ()
deleteTodo conn tid = execute conn "DELETE FROM todos WHERE id = ?" (Only tid)

-- | Toggle completion status of a todo
toggleTodoComplete :: Connection -> TodoId -> IO ()
toggleTodoComplete conn tid = do
    timeStr <- formatCurrentTime
    execute conn
        "UPDATE todos SET completed = NOT completed, \
        \completed_at = CASE WHEN completed = 0 THEN ? ELSE NULL END WHERE id = ?"
        (timeStr, tid)

-- | Helper function to format current time
formatCurrentTime :: IO String
formatCurrentTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" <$> getCurrentTime
