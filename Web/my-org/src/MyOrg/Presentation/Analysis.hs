-- | Display projections retain the public text contract without parsing prose
-- back into domain decisions.
module MyOrg.Presentation.Analysis
  ( AnalysisView (..)
  , ResourceHolderView (..)
  , RecommendationView (..)
  , presentAnalysis
  , renderAnalysis
  , renderCause
  , renderResource
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import MyOrg.Domain.Analysis
import MyOrg.Domain.Identity
import MyOrg.Domain.Result

data ResourceHolderView = ResourceHolderView
  { resource     :: Text
  , required     :: Bool
  , ownerHas     :: Bool
  , controlledBy :: [UserId]
  }
  deriving (Show, Eq)
data RecommendationView = IncreaseAuthorityView UserId [Text]
                        | MoveAccountabilityView UserId
                        | AssignOwnerView
                        | NoStructuralIssueView
  deriving (Show, Eq)
data AnalysisView = AnalysisView
  { goal            :: GoalId
  , owner           :: Maybe UserId
  , coverage        :: Double
  , status          :: Maybe GoalStatus
  , resources       :: [ResourceHolderView]
  , possibleCause   :: Text
  , recommendations :: [RecommendationView]
  }
  deriving (Show, Eq)

presentAnalysis :: Analysis -> AnalysisView
presentAnalysis Analysis {..} =
  AnalysisView
    analysisGoal
    analysisOwner
    analysisCoverage
    analysisStatus
    (map presentHolder analysisResources)
    (renderCause analysisCause)
    (map presentRecommendation analysisRecommendations)
  where
    presentHolder ResourceHolder {..} =
      ResourceHolderView
        (renderResource holderResource)
        holderRequired
        holderOwnerHas
        holderControlledBy
    presentRecommendation = \case
      IncreaseOwnerAuthority uid missing -> IncreaseAuthorityView uid (map resourceName missing)
      MoveAccountabilityUpward uid -> MoveAccountabilityView uid
      AssignOwner -> AssignOwnerView
      NoStructuralIssue -> NoStructuralIssueView
    resourceName (BudgetRequired _) = "Budget"
    resourceName requirement        = renderResource requirement

renderResource :: ResourceRequirement -> Text
renderResource (PermissionRequired permission) = T.pack (show permission)
renderResource (BudgetRequired budget) = "Budget " <> T.pack (show (unMoney budget))

renderCause :: AnalysisCause -> Text
renderCause = \case
  OwnerMissing -> "이 목표에는 최종 책임자가 없습니다. 누구도 지연에 대해 답할 위치에 있지 않습니다."
  ResourcesControlled uid ->
    unUserId uid
      <> "이(가) 목표 달성에 필요한 자원을 모두 통제하고 있습니다. "
      <> "구조적 병목은 발견되지 않았으며, 실행 자체를 점검해야 합니다."
  InsufficientAuthority uid description share ->
    unUserId uid
      <> " owns "
      <> description
      <> ". However, "
      <> unUserId uid
      <> " controls only "
      <> pct share
      <> " of the resources required to achieve the assigned goal."

pct :: Double -> Text
pct x = T.pack (show (round (x * 100) :: Int)) <> "%"

renderAnalysis :: Analysis -> Text
renderAnalysis analysis =
  let a = presentAnalysis analysis
   in T.unlines $
        ["Possible cause", possibleCause a, ""]
          ++ [ resource h
                 <> " authority -> "
                 <> ( if null (controlledBy h)
                        then "(nobody)"
                        else T.intercalate ", " (map unUserId (controlledBy h))
                    )
             | h <- resources a
             ]
          ++ ["", "Recommendation:"]
          ++ zipWith
            (\i r -> T.pack (show (i :: Int)) <> ". " <> renderRec r)
            [1 ..]
            (recommendations a)
  where
    renderRec = \case
      IncreaseAuthorityView uid missing -> "increase " <> unUserId uid <> " authority (" <> T.intercalate ", " missing <> ")"
      MoveAccountabilityView uid -> "move accountability for this goal upward from " <> unUserId uid
      AssignOwnerView -> "assign a final owner"
      NoStructuralIssueView -> "no structural change needed"
