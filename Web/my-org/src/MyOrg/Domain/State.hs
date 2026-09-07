module MyOrg.Domain.State
  ( OrgState (..)
  , emptyState
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import MyOrg.Domain.Authority
import MyOrg.Domain.Discovery
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Identity
import MyOrg.Domain.Organization
import MyOrg.Domain.Result
import MyOrg.Domain.Review.Types

-- | 이벤트를 접어 만든 현재 상태.
data OrgState = OrgState
  { stateOrganization   :: Maybe Organization
  , statePeople         :: Map UserId Person
  , stateProfiles       :: Map UserId EmployeeProfile
  , stateInactivePeople :: Set UserId
  , stateGoals          :: Map GoalId Goal
  , stateOwnership      :: Map GoalId Ownership
  , stateAuthorities    :: Map UserId Authority
  , stateActive         :: Set GoalId
  , stateResults        :: Map GoalId [Result]
    -- ^ 최신 결과가 앞에 온다
  , stateEvaluations    :: Map GoalId Evaluation
  , stateReviews        :: [Review]
    -- ^ 최신 리뷰가 앞에 온다
  , stateStrategies     :: Map GoalId [(UTCTime, Text)]
  , stateDiscovery      :: Discovery
  , stateLastSeq        :: Int
  }
  deriving stock (Show, Eq, Generic)

emptyState :: OrgState
emptyState =
  OrgState
    { stateOrganization = Nothing
    , statePeople = Map.empty
    , stateProfiles = Map.empty
    , stateInactivePeople = Set.empty
    , stateGoals = Map.empty
    , stateOwnership = Map.empty
    , stateAuthorities = Map.empty
    , stateActive = Set.empty
    , stateResults = Map.empty
    , stateEvaluations = Map.empty
    , stateReviews = []
    , stateStrategies = Map.empty
    , stateDiscovery = emptyDiscovery
    , stateLastSeq = 0
    }
