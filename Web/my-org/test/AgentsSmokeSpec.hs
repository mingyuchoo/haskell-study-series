module AgentsSmokeSpec
  ( spec
  ) where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import MyOrg.Server (application)
import MyOrg.Store (closeStore, openFileStore)
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Network.Wai.Handler.Warp (testWithApplication)
import SmokeSupport
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "HTTP agent design" $ do
  it "derives drafts from stored workflows, diagnoses them and saves a reviewed design" $ withFreshServer $ \client -> do
    setup client
    void (post client "organizations/b" empty 404)
    report <- get client route
    let drafts = items (field report "drafts")
    map (`field` "id") drafts `shouldBe` map String ["agent-w-intake", "agent-w-refund"]
    field (first drafts) "handoffTo" `shouldBe` toArray [String "agent-w-refund"]
    field (first drafts) "permissionLevel" `shouldBe` String "L1"
    field (drafts !! 1) "approvalBy" `shouldBe` object ["person" .= String "lead"]
    map (`field` "code") (items (field report "draftDiagnostics"))
      `shouldContain` [String "A009"]
    items (field report "agents") `shouldBe` []
    other <- get client "organizations/b/agents"
    items (field other "drafts") `shouldBe` []
    -- Save the drafts with a human edit: lower the refund agent to L1 and confirm evidence.
    let reviewed =
          map
            (\draft -> merge draft (object ["permissionLevel" .= String "L1", "approvalBy" .= Null]))
            drafts
    void
      ( post
          client
          route
          (object ["expectedVersion" .= field report "version", "agents" .= reviewed])
          201
      )
    saved <- get client route
    map (`field` "permissionLevel") (items (field saved "agents"))
      `shouldBe` [String "L1", String "L1"]
    field saved "version" `shouldSatisfy` (/= field report "version")
    void
      ( post
          client
          route
          (object ["expectedVersion" .= field report "version", "agents" .= reviewed])
          409
      )
    let alone patch = [merge (first drafts) (merge (object ["handoffTo" .= ([] :: [Value])]) patch)]
    void
      ( post
          client
          route
          ( object
              [ "expectedVersion" .= field saved "version"
              , "agents" .= alone (object ["approvalBy" .= object ["person" .= String "ghost"]])
              ]
          )
          404
      )
    void
      ( post
          client
          route
          ( object
              [ "expectedVersion" .= field saved "version"
              , "agents" .= alone (object ["handoffTo" .= [String "agent-missing"]])
              ]
          )
          400
      )
    void
      ( post
          client
          route
          ( object
              [ "expectedVersion" .= field saved "version"
              , "agents" .= alone (object ["permissionLevel" .= String "L9"])
              ]
          )
          400
      )
    get client route `shouldReturn` saved
    (status, contentType, body) <- raw client "organizations/a/agents/export"
    status `shouldBe` 200
    contentType `shouldSatisfy` BS.isPrefixOf "text/markdown"
    body `shouldSatisfy` T.isInfixOf "name: agent-w-intake"
    body `shouldSatisfy` T.isInfixOf "사람이 검토해 저장한 설계"
    (missing, _, _) <- raw client "organizations/missing/agents/export"
    missing `shouldBe` 404
  it "restores the saved design and workflow references after reopening" $
    withSystemTempDirectory "my-org-agents-" $ \directory -> do
      let withServer action = bracket (openFileStore (directory </> "events.json")) closeStore $ \store ->
            testWithApplication (pure (application store)) $ \port -> withClient port action
      withServer $ \client -> do
        setup client
        report <- get client route
        void
          ( post
              client
              route
              (object ["expectedVersion" .= field report "version", "agents" .= field report "drafts"])
              201
          )
      withServer $ \client -> do
        report <- get client route
        map (`field` "id") (items (field report "agents"))
          `shouldBe` map String ["agent-w-intake", "agent-w-refund"]
        survey <- get client "organizations/a/discovery"
        map
          (`field` "handoffWorkflows")
          (take 1 (items (field (field survey "discovery") "workflows")))
          `shouldBe` [toArray [String "w-refund"]]
  it
    "rejects workflow references to unknown people, inactive people, itself and missing workflows" $ withFreshServer $ \client -> do
    setup client
    survey <- get client "organizations/a/discovery"
    let workflows = items (field (field survey "discovery") "workflows")
        withFirst patch =
          merge
            (field survey "discovery")
            (object ["workflows" .= (merge (first workflows) patch : drop 1 workflows)])
        save value status =
          void
            ( post
                client
                "organizations/a/discovery"
                (object ["expectedVersion" .= field survey "version", "discovery" .= value])
                status
            )
    save (withFirst (object ["rolePerson" .= String "ghost"])) 404
    save (withFirst (object ["approvalPerson" .= String "ghost"])) 404
    save (withFirst (object ["handoffWorkflows" .= [String "w-intake"]])) 400
    save (withFirst (object ["handoffWorkflows" .= [String "w-missing"]])) 400
    save (withFirst (object ["approvalPermission" .= String "Everything"])) 400
    get client "organizations/a/discovery" `shouldReturn` survey

route :: String
route = "organizations/a/agents"

setup :: Client -> IO ()
setup client = do
  void
    (post client "organizations" (object ["id" .= String "a", "name" .= String "가상 조직 A"]) 201)
  void
    (post client "organizations" (object ["id" .= String "b", "name" .= String "가상 조직 B"]) 201)
  void
    ( post
        client
        "organizations/a/people"
        (object ["id" .= String "lead", "name" .= String "팀장", "role" .= String "고객지원 팀장"])
        201
    )
  survey <- get client "organizations/a/discovery"
  void
    ( post
        client
        "organizations/a/discovery"
        ( object
            [ "expectedVersion" .= field survey "version"
            , "discovery"
                .= object
                  [ "scope" .= String "고객지원팀"
                  , "asOf" .= String ""
                  , "observations" .= ([] :: [Value])
                  , "workflows"
                      .= [ workflow "w-intake" "문의 접수" "CRM" "분류" "환불은 환불 검토로" [String "w-refund"] "" Nothing
                         , workflow "w-refund" "환불 검토" "결제 콘솔" "환불 기록" "" [] "집행 전 팀장 승인" (Just "lead")
                         ]
                  , "review" .= object ["status" .= String "pending", "note" .= String ""]
                  ]
            ]
        )
        201
    )

workflow
  :: String
  -> String
  -> String
  -> String
  -> String
  -> [Value]
  -> String
  -> Maybe String
  -> Value
workflow ident name tools outputs handoff handoffWorkflows approval approver =
  object
    ( [ "id" .= ident
      , "name" .= name
      , "role" .= String ""
      , "trigger" .= String "접수"
      , "inputs" .= String "문의"
      , "tools" .= tools
      , "outputs" .= outputs
      , "handoff" .= handoff
      , "handoffWorkflows" .= handoffWorkflows
      , "approval" .= approval
      , "status" .= String "confirmed"
      , "evidence" .= String "가상 인터뷰"
      ]
        <> maybe [] (\uid -> ["approvalPerson" .= uid]) approver
    )

raw :: Client -> String -> IO (Int, BS.ByteString, T.Text)
raw (manager, port) path = do
  initial <- parseRequest ("http://127.0.0.1:" <> show port <> "/api/" <> path)
  response <- httpLbs initial manager
  pure
    ( statusCode (responseStatus response)
    , maybe "" id (lookup "Content-Type" (responseHeaders response))
    , TE.decodeUtf8 (BL.toStrict (responseBody response))
    )

merge :: Value -> Value -> Value
merge (Object original) (Object additions) = Object (additions <> original)
merge _ _                                  = error "Expected objects"

toArray :: [Value] -> Value
toArray values = field (object ["values" .= values]) "values"
