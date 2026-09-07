-- | Pure organizational diagnostics. Presentation owns all display text.
module MyOrg.Domain.Compiler
  ( Severity (..)
  , Diagnostic (..)
  , DiagnosticSubject (..)
  , DiagnosticMessage (..)
  , CompileReport (..)
  , compileOrganization
  ) where

import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import MyOrg.Domain.Authority
import MyOrg.Domain.Error
import MyOrg.Domain.Evaluation (evaluateGoal)
import MyOrg.Domain.Goal (missingPermissions, validateDraft)
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Graph
import MyOrg.Domain.Identity
import MyOrg.Domain.Queries
import MyOrg.Domain.Result
import MyOrg.Domain.Review.Types
import MyOrg.Domain.State

data Severity = Error | Warning | Info
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

data DiagnosticSubject = OrganizationSubject
                       | GoalSubject GoalId Text
                       | GoalIdSubject GoalId
                       | PersonSubject UserId
                       | MetricSubject MetricId
                       | ReviewSubject ReviewId
  deriving stock (Show, Eq, Generic)

data DiagnosticMessage = OrganizationMissing
                       | TargetEqualsBaseline
                       | DeadlinePrecedesStart
                       | InvalidDraft OrganizationError
                       | FinalOwnerMissing
                       | UnknownOwner UserId
                       | AuthorityMissing GoalId Text
                       | AuthorityInsufficient Text Double Text Double [Permission] Money Money
                       | SharedMetricOwnership [(GoalId, UserId)]
                       | OwnerOverloaded Int
                       | DecisionConcentration UserId Double
                       | ReviewWithoutOutcome GoalId
                       | ActiveGoalWithoutResult
                       | GoalPastDeadline
  deriving stock (Show, Eq, Generic)

data Diagnostic = Diagnostic
  { diagnosticCode     :: Text
  , diagnosticSeverity :: Severity
  , diagnosticSubject  :: DiagnosticSubject
  , diagnosticMessage  :: DiagnosticMessage
  }
  deriving stock (Show, Eq, Generic)

data CompileReport = CompileReport
  { reportErrors      :: Int
  , reportWarnings    :: Int
  , reportInfos       :: Int
  , reportDiagnostics :: [Diagnostic]
  }
  deriving stock (Show, Eq, Generic)

compileOrganization :: UTCTime -> OrgState -> CompileReport
compileOrganization now st = CompileReport (count Error) (count Warning) (count Info) ordered
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
    ordered = [d | severity <- [Error, Warning, Info], d <- diags, diagnosticSeverity d == severity]
    count severity = length [() | d <- diags, diagnosticSeverity d == severity]

goalSubject :: Goal -> DiagnosticSubject
goalSubject g = GoalSubject (goalId g) (goalDescription g)

checkOrganization :: OrgState -> [Diagnostic]
checkOrganization st =
  [ Diagnostic "O000" Error OrganizationSubject OrganizationMissing
  | stateOrganization st == Nothing
  ]

checkDrafts :: OrgState -> [Diagnostic]
checkDrafts st = mapMaybe check (Map.elems (stateGoals st))
  where
    check g = case validateDraft g of
      Right () -> Nothing
      Left (InvalidTarget _) -> Just (Diagnostic "O002" Error (goalSubject g) TargetEqualsBaseline)
      Left (DeadlineBeforeStart _) -> Just (Diagnostic "O003" Error (goalSubject g) DeadlinePrecedesStart)
      Left err -> Just (Diagnostic "O009" Error (goalSubject g) (InvalidDraft err))

checkOwners :: OrgState -> [Diagnostic]
checkOwners st = [Diagnostic "O001" Error (goalSubject g) FinalOwnerMissing | g <- goalsWithoutOwner st]

checkUnknownOwners :: OrgState -> [Diagnostic]
checkUnknownOwners st =
  [ Diagnostic "O010" Error (GoalIdSubject gid) (UnknownOwner uid)
  | (gid, own) <- Map.toList (stateOwnership st)
  , let uid = ownershipOwner own
  , not (Map.member uid (statePeople st))
  ]

checkAuthority :: OrgState -> [Diagnostic]
checkAuthority st =
  [ case Map.lookup uid (stateAuthorities st) of
      Nothing ->
        Diagnostic
          "O018"
          Error
          (PersonSubject uid)
          (AuthorityMissing (goalId g) (goalDescription g))
      Just authority ->
        Diagnostic
          "O017"
          Warning
          (PersonSubject uid)
          ( AuthorityInsufficient
              (goalDescription g)
              (goalTarget g)
              (metricUnit (goalMetric g))
              coverage
              (Set.toList (missingPermissions g authority))
              (authorityBudgetLimit authority)
              (goalRequiredBudget g)
          )
  | (g, uid, coverage) <- ownersLackingAuthority st
  ]

checkSharedMetrics :: OrgState -> [Diagnostic]
checkSharedMetrics st =
  [ Diagnostic "O020" Warning (MetricSubject mid) (SharedMetricOwnership owners)
  | (mid, owners) <- sharedMetricOwners st
  ]

checkOverload :: OrgState -> [Diagnostic]
checkOverload st =
  [ Diagnostic "O021" Warning (PersonSubject uid) (OwnerOverloaded count)
  | (uid, count) <- Map.toList counts
  , count > 3
  ]
  where
    counts = Map.fromListWith (+) [(ownershipOwner own, 1) | own <- Map.elems (stateOwnership st)]

checkConcentration :: OrgState -> [Diagnostic]
checkConcentration st =
  [ Diagnostic "O031" Warning (PersonSubject uid) (DecisionConcentration uid share)
  | (uid, share) <- Map.toList (decisionShare st)
  , share > 0.5
  , Map.size (stateAuthorities st) > 1
  ]

checkReviews :: OrgState -> [Diagnostic]
checkReviews st =
  [ Diagnostic
      "O040"
      Warning
      (ReviewSubject (reviewId review))
      (ReviewWithoutOutcome (reviewGoal review))
  | review <- stateReviews st
  , null (reviewDecisions review)
  , null (reviewLearnings review)
  ]

checkResults :: OrgState -> [Diagnostic]
checkResults st =
  [ Diagnostic "O050" Info (goalSubject g) ActiveGoalWithoutResult
  | g <- activeGoals st
  , null (resultsOf st (goalId g))
  ]

checkDeadlines :: UTCTime -> OrgState -> [Diagnostic]
checkDeadlines now st =
  [ Diagnostic "O051" Warning (goalSubject g) GoalPastDeadline
  | g <- activeGoals st
  , goalDeadline g < now
  , evaluationStatus (evaluateGoal now g (resultsOf st (goalId g))) /= Achieved
  ]
