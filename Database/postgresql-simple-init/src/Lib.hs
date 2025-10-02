module Lib
  ( runApp
  ) where

-- | 애플리케이션 조립/부트스트랩 모듈
-- 도메인/애플리케이션/어댑터/인프라 계층을 연결하고, 서버를 기동한다.

import           Control.Monad                  (void)
import           Network.Wai                    (Application, responseLBS)
import qualified Network.Wai                    as Wai
import           Network.HTTP.Types             (status200, status404)
import           Network.Wai.Handler.Warp       (run)
import qualified Data.ByteString.Lazy.Char8     as LBS

import           Application.UseCases           (seedSampleData, listAllTests)
import           Adapters.PostgresRepository    (PostgresRepo, initSchema, runWith)
import           Infrastructure.Postgres                 (loadConnectInfoFromEnv, withConnection)
import           Domain.Model                   (TestField(..))

-- | 간단한 라우팅: /health, /tests 두 엔드포인트 제공
mkApp :: (forall a. PostgresRepo a -> IO a) -> Application
mkApp runRepoAction req respond = case (requestMethod, pathInfo) of
  ("GET", [])                -> respond $ ok "OK"
  ("GET", ["health"])       -> respond $ ok "healthy"
  ("GET", ["tests"])        -> do
      tests <- runRepoAction (listAllTests :: PostgresRepo [TestField])
      respond $ ok (LBS.pack (unlines (map render tests)))
  _                           -> respond $ notFound
  where
    requestMethod = Wai.requestMethod req
    pathInfo      = Wai.pathInfo req
    ok msg        = responseLBS status200 [("Content-Type","text/plain; charset=utf-8")] msg
    notFound      = responseLBS status404 [("Content-Type","text/plain")]
                     (LBS.pack "not found")
    render (TestField i n) = show i <> ": " <> n

-- | 애플리케이션 실행
runApp :: IO ()
runApp = do
  -- 1) 환경변수로부터 DB 접속정보 로드
  ci <- loadConnectInfoFromEnv
  -- 2) 커넥션을 열고 스키마 초기화 및 시드 데이터 삽입
  withConnection ci $ \conn -> do
    initSchema conn
    -- 샘플 데이터 삽입 (id=1 upsert 성격)
    void $ runWith conn (seedSampleData :: PostgresRepo Bool)
  -- 3) HTTP 서버 기동
  putStrLn "[boot] 서버 시작: http://0.0.0.0:8000"
  withConnection ci $ \conn -> do
    let runRepoAction act = runWith conn act
    run 8000 (mkApp runRepoAction)
