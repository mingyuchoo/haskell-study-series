-- | Survey data is separate from operational goals and granted authority.
module MyOrg.Domain.Discovery
  ( KnowledgeStatus (..)
  , Observation (..)
  , Workflow (..)
  , DiscoveryReviewStatus (..)
  , DiscoveryReview (..)
  , Discovery (..)
  , emptyDiscovery
  , sameDiscoveryContent
  ) where

import Data.Text (Text)

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
data Workflow = Workflow
  { workflowId       :: Text
  , workflowName     :: Text
  , workflowRole     :: Text
  , workflowTrigger  :: Text
  , workflowInputs   :: Text
  , workflowTools    :: Text
  , workflowOutputs  :: Text
  , workflowHandoff  :: Text
  , workflowApproval :: Text
  , workflowStatus   :: KnowledgeStatus
  , workflowEvidence :: Text
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

sameDiscoveryContent :: Discovery -> Discovery -> Bool
sameDiscoveryContent a b =
  a {discoveryReview = DiscoveryReview Pending ""}
    == b {discoveryReview = DiscoveryReview Pending ""}
