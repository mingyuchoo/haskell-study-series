-- | Organization Compiler.
--
-- 조직 정의를 입력받아 오류와 경고를 진단한다.
--
-- > Organization Definition -> Validator -> Warnings / Errors
module MyOrg.Domain.Compiler
  ( Severity (..)
  , Diagnostic (..)
  , CompileReport (..)
  , compileOrganization
  , DiagnosticMessage (..)
  ) where

import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import MyOrg.Domain.Evaluation (evaluateGoal)
import MyOrg.Domain.State
import MyOrg.Domain.Queries
import MyOrg.Domain.Goal (authorityCoverage, missingPermissions, validateDraft)
import MyOrg.Domain.Graph
import MyOrg.Domain.Identity
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Authority
import MyOrg.Domain.Result
import MyOrg.Domain.Review.Types
import MyOrg.Domain.Error

data Severity = Error | Warning | Info
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

-- Invalid drafts retain structured errors until the presentation boundary.
data DiagnosticMessage = PlainMessage Text | InvalidDraft OrganizationError
  deriving (Show, Eq)

diagnostic :: Text -> Severity -> Text -> Text -> [Text] -> Diagnostic
diagnostic code severity subject message details = Diagnostic code severity subject (PlainMessage message) details

data Diagnostic = Diagnostic
  { diagnosticCode :: Text
  , diagnosticSeverity :: Severity
  , diagnosticSubject :: Text
  -- ^ 목표, 사람 등 진단 대상
  , diagnosticMessage :: DiagnosticMessage
  , diagnosticDetails :: [Text]
  }
  deriving stock (Show, Eq, Generic)



data CompileReport = CompileReport
  { reportErrors :: Int
  , reportWarnings :: Int
  , reportInfos :: Int
  , reportDiagnostics :: [Diagnostic]
  }
  deriving stock (Show, Eq, Generic)



-- | 조직 전체를 검사한다. 오류가 앞에, 경고와 정보가 뒤에 온다.
compileOrganization :: UTCTime -> OrgState -> CompileReport
compileOrganization now st =
  CompileReport
    { reportErrors = count Error
    , reportWarnings = count Warning
    , reportInfos = count Info
    , reportDiagnostics = ordered
    }
 where
  diags =
    concat
      [ checkOrganization st
      , checkDrafts st
      , checkOwners st
      , checkUnknownOwners st
      , checkAuthority st
      , checkSharedMetrics st
      , checkOverload st
      , checkConcentration st
      , checkReviews st
      , checkResults st
      , checkDeadlines now st
      ]
  ordered = [d | s <- [Error, Warning, Info], d <- diags, diagnosticSeverity d == s]
  count s = length [() | d <- diags, diagnosticSeverity d == s]

showGoal :: Goal -> Text
showGoal g = unGoalId (goalId g) <> " \"" <> goalDescription g <> "\""

pct :: Double -> Text
pct x = T.pack (show (round (x * 100) :: Int)) <> "%"

-- O000: 조직 자체가 없음
checkOrganization :: OrgState -> [Diagnostic]
checkOrganization st = case stateOrganization st of
  Just _ -> []
  Nothing ->
    [diagnostic "O000" Error "organization" "조직이 정의되지 않았습니다." []]

-- O002/O003: 초안의 정합성
checkDrafts :: OrgState -> [Diagnostic]
checkDrafts st = mapMaybe check (Map.elems (stateGoals st))
 where
  check g = case validateDraft g of
    Right () -> Nothing
    Left (InvalidTarget _) ->
      Just (diagnostic "O002" Error (showGoal g) "목표값이 기준값과 같아 성공과 실패를 판단할 수 없습니다." [])
    Left (DeadlineBeforeStart _) ->
      Just (diagnostic "O003" Error (showGoal g) "마감이 시작일보다 앞섭니다." [])
    Left e -> Just (Diagnostic "O009" Error (showGoal g) (InvalidDraft e) [])

-- O001: 최종 책임자 없음
checkOwners :: OrgState -> [Diagnostic]
checkOwners st =
  [ diagnostic "O001" Error (showGoal g) "Final Owner가 존재하지 않습니다." []
  | g <- goalsWithoutOwner st
  ]

-- O010: 책임자가 구성원 명단에 없음
checkUnknownOwners :: OrgState -> [Diagnostic]
checkUnknownOwners st =
  [ diagnostic "O010" Error (unGoalId gid)
      ("책임자 " <> unUserId uid <> "이(가) 구성원 명단에 없습니다.") []
  | (gid, o) <- Map.toList (stateOwnership st)
  , let uid = ownershipOwner o
  , not (Map.member uid (statePeople st))
  ]

-- O017/O018: 책임에 비해 권한 부족
checkAuthority :: OrgState -> [Diagnostic]
checkAuthority st =
  [ diag
  | (g, uid, coverage) <- ownersLackingAuthority st
  , let diag = case Map.lookup uid (stateAuthorities st) of
          Nothing ->
            diagnostic "O018" Error (unUserId uid)
              ("목표 " <> showGoal g <> "의 책임자이지만 권한 기록이 전혀 없습니다.")
              []
          Just a ->
            diagnostic "O017" Warning (unUserId uid)
              "책임에 비해 권한이 부족합니다."
              ( [ "Responsibility: " <> goalDescription g <> " = " <> T.pack (show (goalTarget g))
                    <> " " <> metricUnit (goalMetric g)
                , "Controls " <> pct coverage <> " of required resources"
                ]
                  ++ [ T.pack (show p) <> " = False"
                     | p <- Set.toList (missingPermissions g a)
                     ]
                  ++ [ "Budget = " <> T.pack (show (unMoney (authorityBudgetLimit a)))
                        <> " (required " <> T.pack (show (unMoney (goalRequiredBudget g))) <> ")"
                     | authorityBudgetLimit a < goalRequiredBudget g
                     ]
              )
  ]

-- O020: 같은 지표를 두 사람이 최종 책임
checkSharedMetrics :: OrgState -> [Diagnostic]
checkSharedMetrics st =
  [ diagnostic "O020" Warning (unMetricId mid)
      "두 명 이상이 동일한 결과를 최종 책임지고 있습니다."
      [unGoalId g <> " -> " <> unUserId u | (g, u) <- owners]
  | (mid, owners) <- sharedMetricOwners st
  ]

-- O021: 한 사람이 너무 많은 목표를 책임짐
checkOverload :: OrgState -> [Diagnostic]
checkOverload st =
  [ diagnostic "O021" Warning (unUserId uid)
      ("한 사람이 " <> T.pack (show n) <> "개의 목표를 최종 책임지고 있습니다.")
      []
  | (uid, n) <- Map.toList counts
  , n > overloadLimit
  ]
 where
  overloadLimit = 3 :: Int
  counts = Map.fromListWith (+) [(ownershipOwner o, 1) | o <- Map.elems (stateOwnership st)]

-- O031: 의사결정권 집중
checkConcentration :: OrgState -> [Diagnostic]
checkConcentration st =
  [ diagnostic "O031" Warning (unUserId uid)
      (unUserId uid <> "이(가) 전체 조직 의사결정 권한의 " <> pct share <> "를 가지고 있습니다.")
      ["Possible bottleneck detected."]
  | (uid, share) <- Map.toList (decisionShare st)
  , share > 0.5
  , Map.size (stateAuthorities st) > 1
  ]

-- O040: 결정도 학습도 없는 리뷰
checkReviews :: OrgState -> [Diagnostic]
checkReviews st =
  [ diagnostic "O040" Warning (unReviewId (reviewId r))
      "This review produced no decision."
      ["Goal: " <> unGoalId (reviewGoal r)]
  | r <- stateReviews st
  , null (reviewDecisions r)
  , null (reviewLearnings r)
  ]

-- O050: 결과가 한 번도 보고되지 않은 활성 목표
checkResults :: OrgState -> [Diagnostic]
checkResults st =
  [ diagnostic "O050" Info (showGoal g) "활성화된 뒤 보고된 결과가 없습니다." []
  | g <- activeGoals st
  , null (resultsOf st (goalId g))
  ]

-- O051: 마감이 지났는데 달성되지 않음
checkDeadlines :: UTCTime -> OrgState -> [Diagnostic]
checkDeadlines now st =
  [ diagnostic "O051" Warning (showGoal g) "마감이 지났지만 목표가 달성되지 않았습니다." []
  | g <- activeGoals st
  , goalDeadline g < now
  , notAchieved g
  ]
 where
  notAchieved g = evaluationStatus (evaluateGoal now g (resultsOf st (goalId g))) /= Achieved

-- 사용하지 않는 import 경고 방지용 (authorityCoverage는 Graph 쪽에서 사용됨)
_unused :: Goal -> Authority -> Double
_unused = authorityCoverage
