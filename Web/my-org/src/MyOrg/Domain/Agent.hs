-- | Agent roles designed from surveyed workflows. Recording an agent role is a
-- design decision only: it never grants tool access or organizational authority.
module MyOrg.Domain.Agent
  ( PermissionLevel (..)
  , ApprovalBy (..)
  , AgentRole (..)
  , allPermissionLevels
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import MyOrg.Domain.Authority (Permission)
import MyOrg.Domain.Discovery (KnowledgeStatus)
import MyOrg.Domain.Identity

-- | 에이전트가 수행할 수 있는 행위의 등급. CLAUDE.md의 L0~L3 체계와 같다.
data PermissionLevel = L0Read | L1Workspace | L2External | L3Forbidden
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

allPermissionLevels :: [PermissionLevel]
allPermissionLevels = [minBound .. maxBound]

-- | 사람 승인 주체. 구성원 한 명이거나 특정 결정 권한을 가진 사람이다.
data ApprovalBy = ApprovalPerson UserId
                | ApprovalPermission Permission
  deriving stock (Show, Eq, Generic)

data AgentRole = AgentRole
  { agentId              :: Text
  , agentName            :: Text
  , agentSourceWorkflow  :: Maybe Text
    -- ^ 이 역할을 도출한 업무 흐름의 ID
  , agentTask            :: Text
  , agentInputs          :: Text
  , agentOutputs         :: Text
  , agentTools           :: [Text]
    -- ^ 허용 도구 후보. 실제 접근 권한은 별도로 부여한다.
  , agentPermissionLevel :: PermissionLevel
  , agentApprovalBy      :: Maybe ApprovalBy
  , agentHandoffTo       :: [Text]
    -- ^ 산출물을 인계할 다른 에이전트 역할 ID
  , agentStatus          :: KnowledgeStatus
  , agentEvidence        :: Text
  }
  deriving stock (Show, Eq, Generic)
