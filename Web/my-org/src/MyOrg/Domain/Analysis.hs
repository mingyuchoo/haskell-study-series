-- | 목표 지연의 구조적 원인 분석.
--
-- Responsibility Graph와 권한 기록을 바탕으로 "왜 이 목표가 계속 늦어지는가"에
-- 규칙 기반으로 답한다. 향후 LLM을 붙일 때 이 결과를 근거 자료로 넘기면 된다.
module MyOrg.Domain.Analysis
  ( Analysis (..)
  , ResourceHolder (..)
  , Recommendation (..)
  , analyzeGoal
  , renderAnalysis
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
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

-- | 필요한 자원 하나와 그것을 실제로 쥔 사람들.
data ResourceHolder = ResourceHolder
  { holderResource     :: Text
  , holderRequired     :: Bool
  , holderOwnerHas     :: Bool
  , holderControlledBy :: [UserId]
  }
  deriving stock (Show, Eq, Generic)

data Recommendation = IncreaseOwnerAuthority UserId [Text]
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
  , analysisPossibleCause   :: Text
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
            { holderResource = T.pack (show p)
            , holderRequired = True
            , holderOwnerHas = maybe False (`hasPermission` p) mAuth
            , holderControlledBy = resourceControllers st p
            }
        | p <- Set.toList required
        ]
          ++ [ ResourceHolder
                 { holderResource = "Budget " <> T.pack (show (unMoney (goalRequiredBudget g)))
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
      missingNames =
        map (T.pack . show) missing
          ++ [ "Budget"
             | maybe True ((< goalRequiredBudget g) . authorityBudgetLimit) mAuth
             , goalRequiredBudget g > Money 0
             ]
      (cause, recs) = case mOwner of
        Nothing ->
          ( "이 목표에는 최종 책임자가 없습니다. 누구도 지연에 대해 답할 위치에 있지 않습니다."
          , [AssignOwner]
          )
        Just owner
          | coverage >= 1 ->
              ( unUserId owner
                  <> "이(가) 목표 달성에 필요한 자원을 모두 통제하고 있습니다. "
                  <> "구조적 병목은 발견되지 않았으며, 실행 자체를 점검해야 합니다."
              , [NoStructuralIssue]
              )
          | otherwise ->
              ( unUserId owner
                  <> " owns "
                  <> goalDescription g
                  <> ". However, "
                  <> unUserId owner
                  <> " controls only "
                  <> pct coverage
                  <> " of the resources required to achieve the assigned goal."
              , [IncreaseOwnerAuthority owner missingNames, MoveAccountabilityUpward owner]
              )
  pure
    Analysis
      { analysisGoal = gid
      , analysisOwner = mOwner
      , analysisCoverage = coverage
      , analysisStatus = status
      , analysisResources = holders
      , analysisPossibleCause = cause
      , analysisRecommendations = recs
      }

pct :: Double -> Text
pct x = T.pack (show (round (x * 100) :: Int)) <> "%"

renderAnalysis :: Analysis -> Text
renderAnalysis a =
  T.unlines $
    ["Possible cause", analysisPossibleCause a, ""]
      ++ [ holderResource h
             <> " authority -> "
             <> ( if null (holderControlledBy h)
                    then "(nobody)"
                    else T.intercalate ", " (map unUserId (holderControlledBy h))
                )
         | h <- analysisResources a
         ]
      ++ ["", "Recommendation:"]
      ++ zipWith
        (\i r -> T.pack (show (i :: Int)) <> ". " <> renderRec r)
        [1 ..]
        (analysisRecommendations a)
  where
    renderRec = \case
      IncreaseOwnerAuthority u ms ->
        "increase " <> unUserId u <> " authority (" <> T.intercalate ", " ms <> ")"
      MoveAccountabilityUpward u -> "move accountability for this goal upward from " <> unUserId u
      AssignOwner -> "assign a final owner"
      NoStructuralIssue -> "no structural change needed"
