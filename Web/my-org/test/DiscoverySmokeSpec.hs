module DiscoverySmokeSpec
  ( spec
  ) where

import Control.Exception (bracket)
import Control.Monad (forM_, void)
import Data.Aeson.KeyMap qualified as KM
import MyOrg.Server (application)
import MyOrg.Store (closeStore, openFileStore, openSQLiteStore)
import Network.Wai.Handler.Warp (testWithApplication)
import SmokeSupport
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "HTTP discovery" $ do
  it "preserves evidence, isolates organizations and invalidates changed reviews" $ withFreshServer $ \client -> do
    create client "a"
    create client "b"
    initial <- get client route
    field (field initial "discovery") "observations" `shouldBe` toArray []
    save client initial document 201
    saved <- get client route
    field saved "discovery" `shouldBe` document
    other <- get client "organizations/b/discovery"
    field (field other "discovery") "workflows" `shouldBe` toArray []
    save client initial document 409
    let reviewed = merge document (object ["review" .= review "reviewed"])
    save client saved reviewed 201
    approved <- get client route
    field (field (field approved "discovery") "review") "status" `shouldBe` String "reviewed"
    let changed = merge reviewed (object ["scope" .= String "변경한 분석 범위"])
    save client approved changed 201
    refreshed <- get client route
    field (field (field refreshed "discovery") "review") "status" `shouldBe` String "pending"
    field (field (field refreshed "discovery") "review") "note" `shouldBe` String "검토 의견"
    void (call client "GET" "organizations/missing/discovery" Nothing 404)
  it
    "rejects malformed statuses, dates, identities and unsupported confirmed facts atomically"
    $ withFreshServer
    $ \client -> do
      create client "a"
      initial <- get client route
      let observation patch = merge document (object ["observations" .= [merge confirmed patch]])
          workflow patch = merge document (object ["workflows" .= [merge unknown patch]])
          invalid =
            [ observation (object ["status" .= String "inferred"])
            , observation (object ["id" .= String " "])
            , observation (object ["evidence" .= String " "])
            , workflow (object ["status" .= String "invalid"])
            , workflow (object ["id" .= String ""])
            , merge document (object ["observations" .= [confirmed, confirmed]])
            , merge document (object ["workflows" .= [unknown, unknown]])
            , merge document (object ["asOf" .= String "2026-02-30"])
            , merge document (object ["review" .= review "approved"])
            ]
      forM_ invalid $ \bad -> save client initial bad 400
      get client route `shouldReturn` initial
      save client initial document 201
  it "reopens persisted discovery with unknown empty fields and original evidence" $
    withSystemTempDirectory "my-org-discovery-" $ \directory -> do
      let withServer action = bracket (openFileStore (directory </> "events.json")) closeStore $ \store ->
            testWithApplication (pure (application store)) $ \port -> withClient port action
      withServer $ \client -> do
        create client "a"
        initial <- get client route
        save client initial document 201
      withServer $ \client -> do
        saved <- get client route
        field saved "discovery" `shouldBe` document
  it "restores discovery through SQLite event decoding after reopening" $
    withSystemTempDirectory "my-org-discovery-sqlite-" $ \directory -> do
      let withServer action = bracket (openSQLiteStore (directory </> "events.sqlite")) closeStore $ \store ->
            testWithApplication (pure (application store)) $ \port -> withClient port action
      withServer $ \client -> do
        create client "a"
        initial <- get client route
        save client initial document 201
      withServer $ \client -> do
        saved <- get client route
        field saved "discovery" `shouldBe` document

route :: String
route = "organizations/a/discovery"

create :: Client -> String -> IO ()
create client identifier =
  void
    ( post
        client
        "organizations"
        (object ["id" .= identifier, "name" .= String "가상 테스트 조직"])
        201
    )

save :: Client -> Value -> Value -> Int -> IO ()
save client current value status =
  void
    ( post
        client
        route
        (object ["expectedVersion" .= field current "version", "discovery" .= value])
        status
    )

document, confirmed, unknown :: Value
document =
  object
    [ "scope" .= String "고객지원팀"
    , "asOf" .= String "2026-09-07"
    , "observations" .= [confirmed]
    , "workflows" .= [unknown]
    , "review" .= review "pending"
    ]
confirmed =
  object
    [ "id" .= String "o-1"
    , "subject" .= String "권한"
    , "detail" .= String "환불 최종 승인자는 팀장"
    , "status" .= String "confirmed"
    , "evidence" .= String "팀 운영 절차 3항"
    ]
unknown =
  object
    [ "id" .= String "w-1"
    , "name" .= String "환불 검토"
    , "role" .= String ""
    , "trigger" .= String "요청 접수"
    , "inputs" .= String ""
    , "tools" .= String ""
    , "outputs" .= String ""
    , "handoff" .= String ""
    , "approval" .= String ""
    , "status" .= String "unknown"
    , "evidence" .= String ""
    ]

review :: String -> Value
review status = object ["status" .= status, "note" .= String "검토 의견"]

merge :: Value -> Value -> Value
merge (Object original) (Object additions) = Object (KM.union additions original)
merge _ _                                  = error "Expected objects"

toArray :: [Value] -> Value
toArray values = field (object ["values" .= values]) "values"
