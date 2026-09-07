-- | Display text for agent design diagnostics and the exported definition file.
module MyOrg.Presentation.Agent
  ( presentAgentDiagnostic
  , permissionLevelLabel
  , renderAgentExport
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import MyOrg.Domain.Agent
import MyOrg.Domain.AgentDraft
import MyOrg.Domain.Discovery (KnowledgeStatus (..))
import MyOrg.Domain.Identity
import MyOrg.Domain.Organization
import MyOrg.Presentation.Diagnostic (DiagnosticView (..))

permissionLevelLabel :: PermissionLevel -> Text
permissionLevelLabel = \case
  L0Read -> "L0 읽기"
  L1Workspace -> "L1 작업 공간 쓰기"
  L2External -> "L2 외부 영향"
  L3Forbidden -> "L3 금지"

statusLabel :: KnowledgeStatus -> Text
statusLabel = \case
  Confirmed -> "확인된 사실"
  Unknown -> "미확인"
  Proposed -> "개선안"

presentAgentDiagnostic :: AgentDiagnostic -> DiagnosticView
presentAgentDiagnostic AgentDiagnostic {..} =
  DiagnosticView
    agentDiagnosticCode
    agentDiagnosticSeverity
    agentDiagnosticAgent
    message
    details
  where
    (message, details) = case agentDiagnosticIssue of
      HandoffUnresolved target -> ("인계 대상 에이전트가 설계에 없습니다.", ["대상: " <> target])
      ApproverUnknown uid -> ("승인 주체가 구성원 명단에 없습니다.", ["구성원: " <> unUserId uid])
      ApproverInactive uid -> ("승인 주체가 비활성 구성원입니다.", ["구성원: " <> unUserId uid])
      ApproverPermissionUnheld permission ->
        ("승인에 필요한 결정 권한을 가진 활성 구성원이 없습니다.", ["권한: " <> T.pack (show permission)])
      ExternalWithoutApproval -> ("외부 영향(L2) 행위인데 사람 승인 주체가 없습니다.", ["승인 주체를 지정하거나 등급을 낮추세요."])
      ForbiddenLevel -> ("금지 등급(L3) 행위는 에이전트에 배정할 수 없습니다.", ["사람이 직접 수행하도록 설계를 바꾸세요."])
      ToolsUnknown -> ("사용 도구가 미확인입니다.", ["도구 접근 범위를 확인한 뒤 기록하세요."])
      SourceWorkflowMissing wid -> ("도출 근거가 된 업무 흐름이 더 이상 없습니다.", ["업무: " <> wid])
      EvidenceUnconfirmed status -> ("근거가 확인되지 않은 역할입니다.", ["정보 구분: " <> statusLabel status])
      HandoffUnknown -> ("산출물의 인계 대상이 미확인입니다.", ["다음 역할 또는 사람을 확인하세요."])

-- | 사람이 검토한 설계를 에이전트 정의 파일 형식으로 내보낸다. 각 역할은
-- YAML 프런트매터와 Markdown 본문으로 구성되며 실제 실행 설정이 아니다.
renderAgentExport :: Maybe Organization -> Bool -> [AgentRole] -> Text
renderAgentExport organization saved agents =
  T.unlines
    ( [ "# 에이전트 역할 정의 · " <> maybe "조직 미확인" organizationName organization
      , ""
      , "- 출처: " <> (if saved then "사람이 검토해 저장한 설계" else "저장된 설계가 없어 규칙 기반 초안을 내보냄")
      , "- 역할 수: " <> T.pack (show (length agents))
      , "- 이 문서는 설계 기록이며 도구 접근 권한이나 실행 권한을 부여하지 않습니다."
      , ""
      ]
        ++ concatMap renderAgent agents
    )
  where
    renderAgent AgentRole {..} =
      [ "## " <> agentId
      , ""
      , "```markdown"
      , "---"
      , "name: " <> agentId
      , "description: " <> agentName <> " · " <> agentTask
      , "tools: " <> (if null agentTools then "미확인" else T.intercalate ", " agentTools)
      , "permission_level: " <> T.take 2 (permissionLevelLabel agentPermissionLevel)
      , "---"
      , ""
      , "# " <> agentName
      , ""
      , "## 담당 업무"
      , "- " <> orUnknown agentTask
      , ""
      , "## 입력"
      , "- " <> orUnknown agentInputs
      , ""
      , "## 출력"
      , "- " <> orUnknown agentOutputs
      , ""
      , "## 권한 등급"
      , "- " <> permissionLevelLabel agentPermissionLevel
      , ""
      , "## 사람 승인"
      , "- " <> maybe "없음 또는 미확인" approval agentApprovalBy
      , ""
      , "## 인계 대상"
      , "- " <> (if null agentHandoffTo then "미확인" else T.intercalate ", " agentHandoffTo)
      , ""
      , "## 근거"
      , "- 정보 구분: " <> statusLabel agentStatus
      , "- " <> orUnknown agentEvidence
      , "```"
      , ""
      ]
    approval = \case
      ApprovalPerson uid -> "구성원 " <> unUserId uid
      ApprovalPermission permission -> T.pack (show permission) <> " 권한 보유자"
    orUnknown value = if T.null (T.strip value) then "미확인" else value
