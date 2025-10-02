module Domain.Repository
  ( TestRepository(..)
  ) where

import Domain.Model (TestField)

-- | 도메인 레포지토리 인터페이스 (포트)
class TestRepository m where
  createTest   :: TestField -> m Bool
  updateTest   :: TestField -> m Bool
  retrieveTest :: Int -> m (Maybe TestField)
  deleteTest   :: Int -> m Bool
  listTests    :: m [TestField]
