-- | 목표 지연의 구조적 원인 분석.
--
-- Responsibility Graph와 권한 기록을 바탕으로 "왜 이 목표가 계속 늦어지는가"에
-- 규칙 기반으로 답한다. 향후 LLM을 붙일 때 이 결과를 근거 자료로 넘기면 된다.
module MyOrg.Domain.Analysis
  ( Analysis (..)
  , ResourceHolder (..)
  , Recommendation (..)
  , analyzeGoal
  , AnalysisCause (..)
  , ResourceRequirement (..)
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import MyOrg.Domain.Authority
import MyOrg.Domain.Error
import MyOrg.Domain.Goal (authorityCoverage, missingPermissions)
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Graph (resourceControllers)
import MyOrg.Domain.Identity
import MyOrg.Domain.Queries
import MyOrg.Domain.Result
import MyOrg.Domain.State

data ResourceRequirement = PermissionRequired Permission
                         | BudgetRequired Money
  deriving stock (Show, Eq, Generic)

data AnalysisCause = OwnerMissing
                   | ResourcesControlled UserId
                   | InsufficientAuthority UserId Text Double
  deriving stock (Show, Eq, Generic)

-- | 필요한 자원 하나와 그것을 실제로 쥔 사람들.
data ResourceHolder = ResourceHolder
  { holderResource     :: ResourceRequirement
  , holderRequired     :: Bool
  , holderOwnerHas     :: Bool
  , holderControlledBy :: [UserId]
  }
  deriving stock (Show, Eq, Generic)

data Recommendation = IncreaseOwnerAuthority UserId [ResourceRequirement]
                    | MoveAccountabilityUpward UserId
                    | AssignOwner
                    | NoStructuralIssue
  deriving stock (Show, Eq, Generic)

data Analysis = Analysis
  { analysisGoal            :: GoalId
  , analysisOwner           :: Maybe UserId
  , analysisCoverage        :: Double
  , analysisStatus          :: Maybe GoalStatus
  , analysisResources       :: [ResourceHolder]
  , analysisCause           :: AnalysisCause
  , analysisRecommendations :: [Recommendation]
  }
  deriving stock (Show, Eq, Generic)

analyzeGoal :: OrgState -> GoalId -> Either OrganizationError Analysis
analyzeGoal st gid = do
  g <- maybe (Left (GoalNotFound gid)) Right (Map.lookup gid (stateGoals st))
  let mOwner = goalOwner st gid
      mAuth = mOwner >>= ownerAuthority st
      coverage = maybe 0 (authorityCoverage g) mAuth
      status = evaluationStatus <$> Map.lookup gid (stateEvaluations st)
      required = goalRequiredPermissions g
      holders =
        [ ResourceHolder
            { holderResource = PermissionRequired p
            , holderRequired = True
            , holderOwnerHas = maybe False (`hasPermission` p) mAuth
            , holderControlledBy = resourceControllers st p
            }
        | p <- Set.toList required
        ]
          ++ [ ResourceHolder
                 { holderResource = BudgetRequired (goalRequiredBudget g)
                 , holderRequired = True
                 , holderOwnerHas = maybe False ((>= goalRequiredBudget g) . authorityBudgetLimit) mAuth
                 , holderControlledBy =
                     [ uid
                     | (uid, a) <- Map.toList (stateAuthorities st)
                     , authorityBudgetLimit a >= goalRequiredBudget g
                     ]
                 }
             | goalRequiredBudget g > Money 0
             ]
      missing = maybe (Set.toList required) (Set.toList . missingPermissions g) mAuth
      missingResources =
        map PermissionRequired missing
          ++ [ BudgetRequired (goalRequiredBudget g)
             | maybe True ((< goalRequiredBudget g) . authorityBudgetLimit) mAuth
             , goalRequiredBudget g > Money 0
             ]
      (cause, recs) = case mOwner of
        Nothing -> (OwnerMissing, [AssignOwner])
        Just owner
          | coverage >= 1 -> (ResourcesControlled owner, [NoStructuralIssue])
          | otherwise ->
              ( InsufficientAuthority owner (goalDescription g) coverage
              , [IncreaseOwnerAuthority owner missingResources, MoveAccountabilityUpward owner]
              )
  pure
    Analysis
      { analysisGoal = gid
      , analysisOwner = mOwner
      , analysisCoverage = coverage
      , analysisStatus = status
      , analysisResources = holders
      , analysisCause = cause
      , analysisRecommendations = recs
      }
