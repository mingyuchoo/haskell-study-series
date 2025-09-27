module Lib
    ( someFunc
    ) where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           Flow ((<|))
-- | Test data type
data TestField = TestField { testId   :: Int
                           , testName :: String
                           }
     deriving (Show)

-- | Test data type instance
instance FromRow TestField where
    fromRow = TestField <$> field <*> field

-- | Test data type instance
instance ToRow TestField where
    toRow (TestField tid tname) = toRow (tid, tname)

-- | ConnectInfo for local PostgreSQL
localPG :: ConnectInfo
localPG = defaultConnectInfo { connectHost = "127.0.0.1"
                             , connectDatabase = "postgres"
                             , connectUser = "postgres"
                             , connectPassword = "postgres"
                             }

-- | Ensure the required table exists
initDB :: Connection -> IO ()
initDB conn = do
    let q = "CREATE TABLE IF NOT EXISTS test (id INT PRIMARY KEY, name TEXT NOT NULL)"
    _ <- execute_ conn q
    pure ()

someFunc :: IO ()
someFunc = do
    conn <- connect localPG
    initDB conn

    cid <- createTest conn
    putStrLn <| "New Test: " <> (show cid)

    updated <- updateTest conn
    putStrLn <| "Ret value: " <> (show updated)

    row <- retrieveTest conn
    mapM_ print row

    deleted <- deleteTest conn
    putStrLn <| "Ret value: " <> (show deleted)

-- | Create a new test
createTest :: Connection -> IO [Only Int]
createTest conn = query conn "INSERT INTO test (id, name) VALUES (?, ?) RETURNING id" <| ((1 :: Int), ("Jacob" :: String))

-- | Update a test
updateTest :: Connection -> IO Bool
updateTest conn = do
    n <- execute conn "UPDATE test SET name = ? WHERE id = ?" <| (("Tomas" :: String), (1 :: Int))
    return <| n > 0

-- | Retrieve a test
retrieveTest :: Connection -> IO  [TestField]
retrieveTest conn = query conn "SELECT id, name FROM test WHERE id = ?" <| (Only (1 :: Int))

-- | Delete a test
deleteTest :: Connection -> IO Bool
deleteTest conn = do
    n <- execute conn "DELETE FROM test WHERE id = ?" <| (Only (1 :: Int))
    return <| n > 0
