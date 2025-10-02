module Adapters.PostgresRepository
  ( PostgresRepo(..)
  , withPostgresRepo
  , initSchema
  , runWith
  ) where

import           Control.Exception      (bracket)
import           Control.Monad          (void)
import           Data.Maybe             (listToMaybe)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           Domain.Model           (TestField(..))
import           Domain.Repository      (TestRepository(..))

-- | 내부 전용: DB 행 매핑
instance FromRow TestField where
  fromRow = TestField <$> field <*> field

instance ToRow TestField where
  toRow (TestField tid tname) = toRow (tid, tname)

-- | 구체 구현을 담는 newtype 래퍼
newtype PostgresRepo a = PostgresRepo { runRepo :: Connection -> IO a }

-- | Functor 인스턴스
instance Functor PostgresRepo where
  fmap f (PostgresRepo g) = PostgresRepo $ \conn -> fmap f (g conn)

-- | Applicative 인스턴스
instance Applicative PostgresRepo where
  pure x = PostgresRepo $ \_ -> pure x
  (PostgresRepo f) <*> (PostgresRepo x) = PostgresRepo $ \conn -> f conn <*> x conn

-- | Monad 인스턴스
instance Monad PostgresRepo where
  (PostgresRepo m) >>= k = PostgresRepo $ \conn -> do
    a <- m conn
    runRepo (k a) conn

-- | typeclass 인스턴스: IO 모나드에서 동작
instance TestRepository (PostgresRepo) where
  createTest tf = PostgresRepo $ \conn -> do
    n <- execute conn "INSERT INTO test (id, name) VALUES (?, ?) ON CONFLICT (id) DO NOTHING" tf
    pure (n > 0)
  updateTest (TestField i n) = PostgresRepo $ \conn -> do
    n' <- execute conn "UPDATE test SET name = ? WHERE id = ?" (n, i)
    pure (n' > 0)
  retrieveTest i = PostgresRepo $ \conn -> do
    rows <- query conn "SELECT id, name FROM test WHERE id = ?" (Only i)
    pure (listToMaybe rows)
  deleteTest i = PostgresRepo $ \conn -> do
    n <- execute conn "DELETE FROM test WHERE id = ?" (Only i)
    pure (n > 0)
  listTests = PostgresRepo $ \conn -> query_ conn "SELECT id, name FROM test ORDER BY id"

-- | 스키마 초기화
initSchema :: Connection -> IO ()
initSchema conn = do
  void $ execute_ conn "CREATE TABLE IF NOT EXISTS test (id INT PRIMARY KEY, name TEXT NOT NULL)"

-- | 커넥션과 함께 실행 도우미
withPostgresRepo :: ConnectInfo -> (Connection -> IO a) -> IO a
withPostgresRepo ci action = bracket (connect ci) close action

-- | 주어진 커넥션으로 레포지토리 동작 실행
runWith :: Connection -> PostgresRepo a -> IO a
runWith conn repo = runRepo repo conn
