{-# LANGUAGE OverloadedStrings #-}

module DB
    ( TodoId
    , TodoRow (..)
    , initDB
    , createTodo
    , createTodoWithFields
    , getAllTodos
    , updateTodo
    , deleteTodo
    , toggleTodoComplete
    ) where

import           Data.Time.Clock        (getCurrentTime)
import           Data.Time.Format       (defaultTimeLocale, formatTime)
import Database.SQLite.Simple
    ( execute,
      execute_,
      lastInsertRowId,
      query_,
      field,
      Only(Only),
      FromRow(..),
      Connection,
      ToRow(..) )

type TodoId = Int

data TodoRow = TodoRow
    { todoId             :: TodoId
    , todoAction         :: String
    , todoCompleted      :: Bool
    , todoCreatedAt      :: String
    , todoSubject        :: Maybe String
    , todoObject         :: Maybe String
    , todoIndirectObject :: Maybe String
    , todoDirectObject   :: Maybe String
    , todoCompletedAt    :: Maybe String
    } deriving (Show)

instance FromRow TodoRow where
    fromRow = TodoRow <$> field <*> field <*> field <*> field 
                      <*> field <*> field <*> field <*> field <*> field

instance ToRow TodoRow where
    toRow (TodoRow tid txt comp created subj obj indObj dirObj compAt) = 
        toRow (tid, txt, comp, created, subj, obj, indObj, dirObj, compAt)

-- 데이터베이스 초기화
initDB :: Connection -> IO ()
initDB conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS todos \
                  \(id INTEGER PRIMARY KEY AUTOINCREMENT, \
                  \ text TEXT NOT NULL, \
                  \ completed INTEGER NOT NULL DEFAULT 0, \
                  \ created_at TEXT NOT NULL, \
                  \ subject TEXT, \
                  \ object TEXT, \
                  \ indirect_object TEXT, \
                  \ direct_object TEXT, \
                  \ completed_at TEXT)"
    
    -- 샘플 데이터 추가 (테이블이 비어있을 때만)
    count <- query_ conn "SELECT COUNT(*) FROM todos" :: IO [Only Int]
    case count of
        [Only 0] -> do
            timestamp <- getCurrentTime
            let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" timestamp
            execute conn "INSERT INTO todos (text, completed, created_at, subject, object, indirect_object, direct_object, completed_at) \
                         \VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
                ("Welcome to Todo Manager!" :: String, False, timeStr, Nothing :: Maybe String, Nothing :: Maybe String, 
                 Nothing :: Maybe String, Nothing :: Maybe String, Nothing :: Maybe String)
            execute conn "INSERT INTO todos (text, completed, created_at, subject, object, indirect_object, direct_object, completed_at) \
                         \VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
                ("Press 'a' to add a new todo" :: String, False, timeStr, Nothing :: Maybe String, Nothing :: Maybe String,
                 Nothing :: Maybe String, Nothing :: Maybe String, Nothing :: Maybe String)
            execute conn "INSERT INTO todos (text, completed, created_at, subject, object, indirect_object, direct_object, completed_at) \
                         \VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
                ("Press Space to toggle completion" :: String, True, timeStr, Nothing :: Maybe String, Nothing :: Maybe String,
                 Nothing :: Maybe String, Nothing :: Maybe String, Just timeStr)
        _ -> return ()

-- Todo 생성 (기본)
createTodo :: Connection -> String -> IO TodoId
createTodo conn text = do
    timestamp <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" timestamp
    execute conn "INSERT INTO todos (text, completed, created_at, subject, object, indirect_object, direct_object, completed_at) \
                 \VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        (text, False, timeStr, Nothing :: Maybe String, Nothing :: Maybe String, 
         Nothing :: Maybe String, Nothing :: Maybe String, Nothing :: Maybe String)
    fromIntegral <$> lastInsertRowId conn

-- Todo 생성 (모든 필드 포함)
createTodoWithFields :: Connection -> String -> Maybe String -> Maybe String -> Maybe String -> IO TodoId
createTodoWithFields conn text subj indObj dirObj = do
    timestamp <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" timestamp
    execute conn "INSERT INTO todos (text, completed, created_at, subject, object, indirect_object, direct_object, completed_at) \
                 \VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        (text, False, timeStr, subj, Nothing :: Maybe String, indObj, dirObj, Nothing :: Maybe String)
    fromIntegral <$> lastInsertRowId conn

-- 모든 Todo 조회
getAllTodos :: Connection -> IO [TodoRow]
getAllTodos conn = 
    query_ conn "SELECT id, text, completed, created_at, subject, object, indirect_object, direct_object, completed_at \
                \FROM todos ORDER BY id DESC"

-- Todo 업데이트
updateTodo :: Connection -> TodoId -> String -> Bool -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> IO ()
updateTodo conn tid text completed subj obj indObj dirObj compAt =
    execute conn "UPDATE todos SET text = ?, completed = ?, subject = ?, object = ?, \
                 \indirect_object = ?, direct_object = ?, completed_at = ? WHERE id = ?"
        (text, completed, subj, obj, indObj, dirObj, compAt, tid)

-- Todo 삭제
deleteTodo :: Connection -> TodoId -> IO ()
deleteTodo conn tid =
    execute conn "DELETE FROM todos WHERE id = ?" (Only tid)

-- Todo 완료 상태 토글
toggleTodoComplete :: Connection -> TodoId -> IO ()
toggleTodoComplete conn tid = do
    timestamp <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" timestamp
    execute conn "UPDATE todos SET completed = NOT completed, \
                 \completed_at = CASE WHEN completed = 0 THEN ? ELSE NULL END WHERE id = ?"
        (timeStr, tid)
