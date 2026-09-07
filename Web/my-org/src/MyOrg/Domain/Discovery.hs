-- | Survey data is separate from operational goals and granted authority.
module MyOrg.Domain.Discovery
  ( KnowledgeStatus (..)
  , Observation (..)
  , Workflow (..)
  , DiscoveryReviewStatus (..)
  , DiscoveryReview (..)
  , Discovery (..)
  , emptyDiscovery
  , emptyWorkflow
  , sameDiscoveryContent
  ) where

import Data.Text (Text)
import MyOrg.Domain.Authority (Permission)
import MyOrg.Domain.Identity (UserId)

data KnowledgeStatus = Confirmed | Unknown | Proposed
  deriving (Show, Eq)
data Observation = Observation
  { observationId       :: Text
  , observationSubject  :: Text
  , observationDetail   :: Text
  , observationStatus   :: KnowledgeStatus
  , observationEvidence :: Text
  }
  deriving (Show, Eq)

-- | 업무 흐름. 자유 텍스트 필드는 그대로 유지하고, 선택적인 참조 필드가
-- 구성원, 결정 권한, 다른 업무를 가리킨다. 참조는 텍스트를 대체하지 않는다.
data Workflow = Workflow
  { workflowId                 :: Text
  , workflowName               :: Text
  , workflowRole               :: Text
  , workflowRolePerson         :: Maybe UserId
  , workflowTrigger            :: Text
  , workflowInputs             :: Text
  , workflowTools              :: Text
  , workflowOutputs            :: Text
  , workflowHandoff            :: Text
  , workflowHandoffWorkflows   :: [Text]
  , workflowApproval           :: Text
  , workflowApprovalPerson     :: Maybe UserId
  , workflowApprovalPermission :: Maybe Permission
  , workflowStatus             :: KnowledgeStatus
  , workflowEvidence           :: Text
  }
  deriving (Show, Eq)

data DiscoveryReviewStatus = Pending | Reviewed
  deriving (Show, Eq)
data DiscoveryReview = DiscoveryReview
  { discoveryReviewStatus :: DiscoveryReviewStatus
  , discoveryReviewNote   :: Text
  }
  deriving (Show, Eq)
data Discovery = Discovery
  { discoveryScope        :: Text
  , discoveryAsOf         :: Text
  , discoveryObservations :: [Observation]
  , discoveryWorkflows    :: [Workflow]
  , discoveryReview       :: DiscoveryReview
  }
  deriving (Show, Eq)

emptyDiscovery :: Discovery
emptyDiscovery = Discovery "" "" [] [] (DiscoveryReview Pending "")

emptyWorkflow :: Text -> Text -> Workflow
emptyWorkflow ident name =
  Workflow ident name "" Nothing "" "" "" "" "" [] "" Nothing Nothing Unknown ""

sameDiscoveryContent :: Discovery -> Discovery -> Bool
sameDiscoveryContent a b =
  a {discoveryReview = DiscoveryReview Pending ""}
    == b {discoveryReview = DiscoveryReview Pending ""}
