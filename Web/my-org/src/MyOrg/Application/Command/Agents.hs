module MyOrg.Application.Command.Agents
  ( saveAgentRoles
  ) where

import Control.Monad (unless, when)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import MyOrg.Application.Command.Validation
import MyOrg.Domain.Agent
import MyOrg.Domain.Discovery (KnowledgeStatus (..))
import MyOrg.Domain.Error
import MyOrg.Domain.Event.Types
import MyOrg.Domain.Queries (requireActivePerson)
import MyOrg.Domain.State

-- | 설계된 역할 목록 전체를 저장한다. 저장은 설계 기록일 뿐이며 실제 실행
-- 권한이나 도구 접근을 부여하지 않는다.
saveAgentRoles
  :: OrgState -> [AgentRole] -> Int -> Either OrganizationError OrganizationEvent
saveAgentRoles st agents version = do
  _ <- requireOrganization st
  checkVersion st version
  when (length agents > 200) (Left (InvalidInput "에이전트 역할은 200개까지 기록할 수 있습니다."))
  let ids = map agentId agents
  mapM_ identifier ids
  unless
    (Set.size (Set.fromList ids) == length ids)
    (Left (InvalidInput "에이전트 역할 ID가 중복되었습니다."))
  mapM_ (validateAgent st (Set.fromList ids)) agents
  pure (AgentRolesSaved agents)

validateAgent :: OrgState -> Set.Set Text -> AgentRole -> Either OrganizationError ()
validateAgent st ids AgentRole {..} = do
  nonempty agentName
  mapM_ bounded [agentTask, agentInputs, agentOutputs, agentEvidence]
  mapM_ bounded agentSourceWorkflow
  when (length agentTools > 50) (Left (InvalidInput "도구는 50개까지 기록할 수 있습니다."))
  mapM_ nonempty agentTools
  mapM_
    ( \target -> do
        when (target == agentId) (Left (InvalidInput "에이전트는 자기 자신에게 인계할 수 없습니다."))
        unless (Set.member target ids) (Left (InvalidInput ("인계 대상 에이전트를 찾을 수 없습니다: " <> target)))
    )
    agentHandoffTo
  case agentApprovalBy of
    Just (ApprovalPerson uid) -> requireActivePerson st uid
    _                         -> pure ()
  when (agentStatus == Confirmed && T.null (T.strip agentEvidence)) (Left (InvalidInput "확인된 사실에는 근거가 필요합니다."))

bounded :: Text -> Either OrganizationError ()
bounded value = when (T.length value > 10000) (Left (InvalidInput "입력은 항목당 10000자 이하여야 합니다."))
