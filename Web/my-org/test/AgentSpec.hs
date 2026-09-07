module AgentSpec
  ( spec
  ) where

import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import MyOrg.Application
import MyOrg.Demo (demoEvents)
import MyOrg.Domain.Agent
import MyOrg.Domain.AgentDraft
import MyOrg.Domain.Compiler (Severity (..))
import MyOrg.Domain.Discovery
import MyOrg.Domain.Event
import MyOrg.Presentation.Agent (presentAgentDiagnostic, renderAgentExport)
import MyOrg.Presentation.Diagnostic (DiagnosticView (..))
import MyOrg.Serialization.Persistence (decodeStoredEvent, encodeStoredEvent)
import MyOrg.Types
import Test.Hspec

start :: UTCTime
start = read "2026-09-07 00:00:00 UTC"

demoState :: IO OrgState
demoState = either (fail . show) (pure . replay) (demoEvents start)

agent :: Text -> AgentRole
agent ident = AgentRole ident ("역할 " <> ident) Nothing "업무" "입력" "산출물" ["CRM"] L1Workspace Nothing [] Confirmed "근거"

byId :: Text -> [AgentRole] -> AgentRole
byId ident agents = case filter ((== ident) . agentId) agents of
  [found] -> found
  _       -> error ("missing agent " <> T.unpack ident)

codesOf :: [AgentDiagnostic] -> [(Text, Text)]
codesOf = map (\d -> (agentDiagnosticCode d, agentDiagnosticAgent d))

spec :: Spec
spec = describe "agent role derivation, diagnostics and design commands" $ do
  it "derives one role per stored workflow using references before text" $ do
    st <- demoState
    let drafts = deriveAgents st
        inquiry = byId "agent-demo-w-inquiry" drafts
        refund = byId "agent-demo-w-refund" drafts
        proposal = byId "agent-demo-w-proposal" drafts
        contract = byId "agent-demo-w-contract" drafts
    map agentId drafts `shouldBe` ["agent-demo-w-inquiry", "agent-demo-w-refund", "agent-demo-w-proposal", "agent-demo-w-contract"]
    (agentName inquiry, agentPermissionLevel inquiry, agentTools inquiry, agentHandoffTo inquiry)
      `shouldBe` ("고객 성공 담당", L1Workspace, ["CRM", "고객지원 문서"], ["agent-demo-w-refund"])
    (agentPermissionLevel refund, agentApprovalBy refund, agentHandoffTo refund)
      `shouldBe` (L2External, Just (ApprovalPerson (UserId "demo-ceo")), [])
    (agentPermissionLevel proposal, agentApprovalBy proposal, agentHandoffTo proposal)
      `shouldBe` (L2External, Just (ApprovalPermission Pricing), ["agent-demo-w-contract"])
    (agentName contract, agentPermissionLevel contract, agentTools contract, agentApprovalBy contract, agentStatus contract)
      `shouldBe` ("파트너십 계약 검토 담당", L0Read, [], Nothing, Unknown)
    map agentSourceWorkflow drafts `shouldBe` map Just ["demo-w-inquiry", "demo-w-refund", "demo-w-proposal", "demo-w-contract"]
  it "falls back to active member names and workflow names mentioned in free text" $ do
    st <- demoState
    let intake =
          (emptyWorkflow "intake" "문의 접수")
            { workflowOutputs = "분류"
            , workflowHandoff = "환불 건은 환불 처리로 넘긴다"
            , workflowApproval = "집행 전 정하린 확인"
            }
        refund = (emptyWorkflow "refund" "환불 처리") {workflowApproval = "박서준 승인"}
        survey = emptyDiscovery {discoveryWorkflows = [intake, refund]}
        drafts = deriveAgents st {stateDiscovery = survey}
        inactive = deriveAgents st {stateDiscovery = survey, stateInactivePeople = Set.singleton (UserId "demo-success")}
    (agentHandoffTo (byId "agent-intake" drafts), agentApprovalBy (byId "agent-intake" drafts))
      `shouldBe` (["agent-refund"], Just (ApprovalPerson (UserId "demo-success")))
    agentApprovalBy (byId "agent-refund" drafts) `shouldBe` Just (ApprovalPerson (UserId "demo-ceo"))
    agentApprovalBy (byId "agent-intake" inactive) `shouldBe` Nothing
    agentPermissionLevel (byId "agent-intake" inactive) `shouldBe` L2External
  it "diagnoses the demo drafts without inventing confirmed facts" $ do
    st <- demoState
    let diagnostics = diagnoseAgents st (deriveAgents st)
    codesOf diagnostics `shouldContain` [("A008", "agent-demo-w-contract")]
    codesOf diagnostics `shouldContain` [("A009", "agent-demo-w-contract")]
    codesOf diagnostics `shouldContain` [("A009", "agent-demo-w-refund")]
    map fst (codesOf diagnostics) `shouldNotContain` ["A001"]
    map fst (codesOf diagnostics) `shouldNotContain` ["A002"]
    map fst (codesOf diagnostics) `shouldNotContain` ["A005"]
    map agentDiagnosticSeverity diagnostics `shouldSatisfy` (\xs -> and (zipWith (<=) xs (drop 1 xs)))
  it "flags unresolved handoffs, approvers, levels, tools and stale sources" $ do
    st <- demoState
    let inactive = st {stateInactivePeople = Set.singleton (UserId "demo-sales")}
        agents =
          [ (agent "a") {agentHandoffTo = ["missing"]}
          , (agent "b") {agentPermissionLevel = L2External, agentApprovalBy = Just (ApprovalPerson (UserId "nobody"))}
          , (agent "c") {agentPermissionLevel = L2External, agentApprovalBy = Just (ApprovalPerson (UserId "demo-sales"))}
          , (agent "d") {agentPermissionLevel = L2External, agentApprovalBy = Just (ApprovalPermission Hiring)}
          , (agent "e") {agentPermissionLevel = L2External}
          , (agent "f") {agentPermissionLevel = L3Forbidden}
          , (agent "g") {agentTools = []}
          , (agent "h") {agentSourceWorkflow = Just "gone"}
          ]
        noHiring = inactive {stateAuthorities = Map.map (revokePermission Hiring) (stateAuthorities inactive)}
        codes = codesOf (diagnoseAgents noHiring agents)
    mapM_ (\expected -> codes `shouldContain` [expected]) [("A001", "a"), ("A002", "b"), ("A002", "c"), ("A003", "d"), ("A004", "e"), ("A005", "f"), ("A006", "g"), ("A007", "h")]
    map fst (codesOf (diagnoseAgents st [(agent "d") {agentPermissionLevel = L2External, agentApprovalBy = Just (ApprovalPermission Hiring)}]))
      `shouldNotContain` ["A003"]
    let view = presentAgentDiagnostic (AgentDiagnostic "A004" Warning "e" ExternalWithoutApproval)
    (code view, severity view, subject view) `shouldBe` ("A004", Warning, "e")
    message view `shouldSatisfy` (not . T.null)
  it "validates the saved design against the organization and version" $ do
    st <- demoState
    let version = stateLastSeq st
        save agents v = executeCommand start st (SaveAgentRoles agents v)
        valid = [(agent "review") {agentHandoffTo = ["execute"]}, (agent "execute") {agentPermissionLevel = L2External, agentApprovalBy = Just (ApprovalPerson (UserId "demo-ceo"))}]
    save valid version `shouldBe` Right [AgentRolesSaved valid]
    save valid (version - 1) `shouldBe` Left (VersionConflict (version - 1) version)
    save [agent "dup", agent "dup"] version `shouldSatisfy` isLeft
    save [(agent "a") {agentHandoffTo = ["missing"]}] version `shouldSatisfy` isLeft
    save [(agent "a") {agentHandoffTo = ["a"]}] version `shouldSatisfy` isLeft
    save [(agent "a") {agentApprovalBy = Just (ApprovalPerson (UserId "nobody"))}] version
      `shouldBe` Left (PersonNotFound (UserId "nobody"))
    save [(agent "a") {agentName = " "}] version `shouldSatisfy` isLeft
    save [(agent "bad id!")] version `shouldSatisfy` isLeft
    save [(agent "a") {agentStatus = Confirmed, agentEvidence = ""}] version `shouldSatisfy` isLeft
    save [] version `shouldBe` Right [AgentRolesSaved []]
    let stored = StoredEvent (version + 1) start Nothing (AgentRolesSaved valid)
    stateAgents (applyEvent st stored) `shouldBe` valid
    decodeStoredEvent (encodeStoredEvent stored) `shouldBe` Right stored
  it "reads workflows persisted before reference fields existed" $ do
    let raw =
          BL.pack
            "{\"seq\":1,\"at\":\"2026-09-07T00:00:00Z\",\"event\":{\"tag\":\"DiscoverySaved\",\"contents\":{\"scope\":\"s\",\"asOf\":\"\",\"observations\":[],\"workflows\":[{\"id\":\"w\",\"name\":\"n\",\"role\":\"\",\"trigger\":\"\",\"inputs\":\"\",\"tools\":\"\",\"outputs\":\"\",\"handoff\":\"\",\"approval\":\"\",\"status\":\"unknown\",\"evidence\":\"\"}],\"review\":{\"status\":\"pending\",\"note\":\"\"}}}}"
        expected = StoredEvent 1 start Nothing (DiscoverySaved emptyDiscovery {discoveryScope = "s", discoveryWorkflows = [emptyWorkflow "w" "n"]})
    decodeStoredEvent raw `shouldBe` Right expected
    -- Re-encoding a workflow without references adds no new keys.
    BL.unpack (encodeStoredEvent expected) `shouldSatisfy` (not . T.isInfixOf "handoffWorkflows" . T.pack)
  it "exports every saved role as a definition block" $ do
    st <- demoState
    let document = renderAgentExport (stateOrganization st) False (deriveAgents st)
    document `shouldSatisfy` T.isInfixOf "name: agent-demo-w-refund"
    document `shouldSatisfy` T.isInfixOf "permission_level: L2"
    document `shouldSatisfy` T.isInfixOf "규칙 기반 초안"
    document `shouldSatisfy` T.isInfixOf "구성원 demo-ceo"
    document `shouldSatisfy` T.isInfixOf "Pricing 권한 보유자"
    renderAgentExport (stateOrganization st) True [] `shouldSatisfy` T.isInfixOf "역할 수: 0"
