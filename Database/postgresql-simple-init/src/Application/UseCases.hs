module Application.UseCases
  ( initializeDatabase
  , seedSampleData
  , listAllTests
  ) where

import           Domain.Model (TestField(..))
import           Domain.Repository

-- | 스키마 준비: 애플리케이션 계층은 인터페이스에만 의존
initializeDatabase :: (TestRepository m, Applicative m) => m Bool
initializeDatabase = pure True  -- 구체 구현은 어댑터/인프라에서 수행

-- | 샘플 데이터 삽입 유스케이스
seedSampleData :: (TestRepository m, Monad m) => m Bool
seedSampleData = do
  _ <- createTest (TestField 1 "Jacob")
  _ <- updateTest (TestField 1 "Tomas")
  pure True

-- | 전체 조회 유스케이스
listAllTests :: (TestRepository m) => m [TestField]
listAllTests = listTests
