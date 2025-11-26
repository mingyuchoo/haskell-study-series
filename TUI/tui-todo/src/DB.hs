{-# LANGUAGE OverloadedStrings #-}

module DB
    ( TodoId
    , TodoRow (..)
    , initDB
    , createTodo
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
    { todoId        :: TodoId
    , todoAction      :: String
    , todoCompleted :: Bool
    , todoCreatedAt :: String
    } deriving (Show)

instance FromRow TodoRow where
    fromRow = TodoRow <$> field <*> field <*> field <*> field

instance ToRow TodoRow where
    toRow (TodoRow tid txt comp created) = toRow (tid, txt, comp, created)

-- 데이터베이스 초기화
initDB :: Connection -> IO ()
initDB conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS todos \
                  \(id INTEGER PRIMARY KEY AUTOINCREMENT, \
                  \ text TEXT NOT NULL, \
                  \ completed INTEGER NOT NULL DEFAULT 0, \
                  \ created_at TEXT NOT NULL)"
    
    -- 샘플 데이터 추가 (테이블이 비어있을 때만)
    count <- query_ conn "SELECT COUNT(*) FROM todos" :: IO [Only Int]
    case count of
        [Only 0] -> do
            timestamp <- getCurrentTime
            let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" timestamp
            execute conn "INSERT INTO todos (text, completed, created_at) VALUES (?, ?, ?)"
                ("Welcome to Todo Manager!" :: String, False, timeStr)
            execute conn "INSERT INTO todos (text, completed, created_at) VALUES (?, ?, ?)"
                ("Press 'a' to add a new todo" :: String, False, timeStr)
            execute conn "INSERT INTO todos (text, completed, created_at) VALUES (?, ?, ?)"
                ("Press Space to toggle completion" :: String, True, timeStr)
        _ -> return ()

-- Todo 생성
createTodo :: Connection -> String -> IO TodoId
createTodo conn text = do
    timestamp <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" timestamp
    execute conn "INSERT INTO todos (text, completed, created_at) VALUES (?, ?, ?)"
        (text, False, timeStr)
    fromIntegral <$> lastInsertRowId conn

-- 모든 Todo 조회
getAllTodos :: Connection -> IO [TodoRow]
getAllTodos conn = 
    query_ conn "SELECT id, text, completed, created_at FROM todos ORDER BY id DESC"

-- Todo 업데이트
updateTodo :: Connection -> TodoId -> String -> Bool -> IO ()
updateTodo conn tid text completed =
    execute conn "UPDATE todos SET text = ?, completed = ? WHERE id = ?"
        (text, completed, tid)

-- Todo 삭제
deleteTodo :: Connection -> TodoId -> IO ()
deleteTodo conn tid =
    execute conn "DELETE FROM todos WHERE id = ?" (Only tid)

-- Todo 완료 상태 토글
toggleTodoComplete :: Connection -> TodoId -> IO ()
toggleTodoComplete conn tid =
    execute conn "UPDATE todos SET completed = NOT completed WHERE id = ?" (Only tid)
