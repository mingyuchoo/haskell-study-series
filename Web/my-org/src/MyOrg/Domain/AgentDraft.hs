-- | Deterministic agent role derivation and design diagnostics. Every rule
-- reads only stored survey data and organization state; nothing is inferred
-- by an external model, and unknown input stays unknown in the draft.
module MyOrg.Domain.AgentDraft
  ( AgentIssue (..)
  , AgentDiagnostic (..)
  , agentKey
  , deriveAgents
  , diagnoseAgents
  , splitItems
  ) where

import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import MyOrg.Domain.Agent
import MyOrg.Domain.Authority
import MyOrg.Domain.Compiler (Severity (..))
import MyOrg.Domain.Discovery
import MyOrg.Domain.Identity
import MyOrg.Domain.Organization
import MyOrg.Domain.State

data AgentIssue = HandoffUnresolved Text
                | ApproverUnknown UserId
                | ApproverInactive UserId
                | ApproverPermissionUnheld Permission
                | ExternalWithoutApproval
                | ForbiddenLevel
                | ToolsUnknown
                | SourceWorkflowMissing Text
                | EvidenceUnconfirmed KnowledgeStatus
                | HandoffUnknown
  deriving stock (Show, Eq, Generic)

data AgentDiagnostic = AgentDiagnostic
  { agentDiagnosticCode     :: Text
  , agentDiagnosticSeverity :: Severity
  , agentDiagnosticAgent    :: Text
  , agentDiagnosticIssue    :: AgentIssue
  }
  deriving stock (Show, Eq, Generic)

-- | 업무 흐름 ID로부터 결정적으로 만드는 에이전트 역할 ID.
agentKey :: Text -> Text
agentKey wid = "agent-" <> wid

-- | 쉼표 또는 줄바꿈으로 구분한 항목 목록.
splitItems :: Text -> [Text]
splitItems = filter (not . T.null) . map T.strip . T.split (\c -> c == ',' || c == '\n' || c == ';')

blank :: Text -> Bool
blank = T.null . T.strip

mentioned :: Text -> Text -> Bool
mentioned needle hay = not (blank needle) && T.strip needle `T.isInfixOf` hay

activePeople :: OrgState -> [Person]
activePeople st =
  [ p
  | p <- Map.elems (statePeople st)
  , not (Set.member (personId p) (stateInactivePeople st))
  ]

-- | 저장된 업무 흐름마다 하나의 역할 후보를 만든다.
--
-- 규칙:
--
-- * 이름: 담당 역할 텍스트, 비어 있으면 "업무 이름 담당".
-- * 권한 등급: 사람 승인 조건이 있으면 L2(외부 영향), 도구가 있으면 L1(작업 공간), 그 외 L0(읽기).
-- * 승인 주체: 참조 필드를 우선 사용하고, 없으면 승인 텍스트에 이름이 포함된 활성 구성원.
-- * 인계 대상: 참조 필드를 우선 사용하고, 없으면 인계 텍스트에 이름이나 역할이 포함된 다른 업무.
-- * 비어 있는 정보는 채우지 않는다. 상태와 근거는 업무 흐름의 값을 그대로 옮긴다.
deriveAgents :: OrgState -> [AgentRole]
deriveAgents st = map derive workflows
  where
    workflows = discoveryWorkflows (stateDiscovery st)
    people = activePeople st
    derive w =
      AgentRole
        { agentId = agentKey (workflowId w)
        , agentName =
            if blank (workflowRole w) then workflowName w <> " 담당" else T.strip (workflowRole w)
        , agentSourceWorkflow = Just (workflowId w)
        , agentTask = workflowName w
        , agentInputs = workflowInputs w
        , agentOutputs = workflowOutputs w
        , agentTools = splitItems (workflowTools w)
        , agentPermissionLevel = level w
        , agentApprovalBy = approval w
        , agentHandoffTo = handoffs w
        , agentStatus = workflowStatus w
        , agentEvidence = workflowEvidence w
        }
    hasApproval w =
      isJust (workflowApprovalPerson w)
        || isJust (workflowApprovalPermission w)
        || not (blank (workflowApproval w))
    level w
      | hasApproval w = L2External
      | not (null (splitItems (workflowTools w))) = L1Workspace
      | otherwise = L0Read
    approval w = case (workflowApprovalPerson w, workflowApprovalPermission w) of
      (Just uid, _) -> Just (ApprovalPerson uid)
      (_, Just p) -> Just (ApprovalPermission p)
      _ ->
        ApprovalPerson . personId
          <$> find (\p -> mentioned (personName p) (workflowApproval w)) people
    handoffs w
      | not (null (workflowHandoffWorkflows w)) = map agentKey (workflowHandoffWorkflows w)
      | otherwise =
          [ agentKey (workflowId other)
          | other <- workflows
          , workflowId other /= workflowId w
          , mentioned (workflowName other) (workflowHandoff w)
              || mentioned (workflowRole other) (workflowHandoff w)
          ]

-- | 설계된 역할 목록을 조직 상태에 비추어 검사한다. 저장 전 초안과 저장된
-- 설계 모두에 같은 규칙을 적용한다.
diagnoseAgents :: OrgState -> [AgentRole] -> [AgentDiagnostic]
diagnoseAgents st agents =
  [ d
  | severity <- [Error, Warning, Info]
  , d <- concatMap check agents
  , agentDiagnosticSeverity d == severity
  ]
  where
    ids = Set.fromList (map agentId agents)
    workflowIds = Set.fromList (map workflowId (discoveryWorkflows (stateDiscovery st)))
    holders permission =
      [ uid
      | (uid, authority) <- Map.toList (stateAuthorities st)
      , hasPermission authority permission
      , not (Set.member uid (stateInactivePeople st))
      ]
    check agent =
      let this = agentId agent
          diag code severity issue = AgentDiagnostic code severity this issue
       in concat
            [ [ diag "A001" Error (HandoffUnresolved target)
              | target <- agentHandoffTo agent
              , not (Set.member target ids)
              ]
            , case agentApprovalBy agent of
                Just (ApprovalPerson uid)
                  | not (Map.member uid (statePeople st)) -> [diag "A002" Error (ApproverUnknown uid)]
                  | Set.member uid (stateInactivePeople st) -> [diag "A002" Error (ApproverInactive uid)]
                Just (ApprovalPermission permission)
                  | null (holders permission) -> [diag "A003" Warning (ApproverPermissionUnheld permission)]
                _ -> []
            , [ diag "A004" Warning ExternalWithoutApproval
              | agentPermissionLevel agent == L2External
              , isNothing (agentApprovalBy agent)
              ]
            , [diag "A005" Error ForbiddenLevel | agentPermissionLevel agent == L3Forbidden]
            , [ diag "A006" Info ToolsUnknown
              | null (agentTools agent)
              , agentPermissionLevel agent /= L0Read
              ]
            , [ diag "A007" Warning (SourceWorkflowMissing wid)
              | Just wid <- [agentSourceWorkflow agent]
              , not (Set.member wid workflowIds)
              ]
            , [ diag "A008" Warning (EvidenceUnconfirmed (agentStatus agent))
              | agentStatus agent /= Confirmed
              ]
            , [ diag "A009" Info HandoffUnknown
              | null (agentHandoffTo agent)
              , not (blank (agentOutputs agent))
              ]
            ]
