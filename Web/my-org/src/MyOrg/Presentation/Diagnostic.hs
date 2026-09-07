module MyOrg.Presentation.Diagnostic
  ( DiagnosticView (..)
  , CompileReportView (..)
  , presentDiagnostic
  , presentCompileReport
  , renderDiagnostic
  , renderDiagnosticMessage
  , renderDiagnosticSubject
  , renderDiagnosticDetails
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import MyOrg.Domain.Compiler
import MyOrg.Domain.Identity
import MyOrg.Presentation.Error (describeError)

data DiagnosticView = DiagnosticView
  { code     :: Text
  , severity :: Severity
  , subject  :: Text
  , message  :: Text
  , details  :: [Text]
  }
  deriving (Show, Eq)
data CompileReportView = CompileReportView
  { errors      :: Int
  , warnings    :: Int
  , infos       :: Int
  , diagnostics :: [DiagnosticView]
  }
  deriving (Show, Eq)

presentDiagnostic :: Diagnostic -> DiagnosticView
presentDiagnostic Diagnostic {..} =
  DiagnosticView
    diagnosticCode
    diagnosticSeverity
    (renderDiagnosticSubject diagnosticSubject)
    (renderDiagnosticMessage diagnosticMessage)
    (renderDiagnosticDetails diagnosticMessage)

presentCompileReport :: CompileReport -> CompileReportView
presentCompileReport CompileReport {..} =
  CompileReportView
    reportErrors
    reportWarnings
    reportInfos
    (map presentDiagnostic reportDiagnostics)

renderDiagnosticSubject :: DiagnosticSubject -> Text
renderDiagnosticSubject = \case
  OrganizationSubject -> "organization"
  GoalSubject gid description -> showGoal gid description
  GoalIdSubject gid -> unGoalId gid
  PersonSubject uid -> unUserId uid
  MetricSubject mid -> unMetricId mid
  ReviewSubject rid -> unReviewId rid

renderDiagnosticMessage :: DiagnosticMessage -> Text
renderDiagnosticMessage = \case
  OrganizationMissing -> "조직이 정의되지 않았습니다."
  TargetEqualsBaseline -> "목표값이 기준값과 같아 성공과 실패를 판단할 수 없습니다."
  DeadlinePrecedesStart -> "마감이 시작일보다 앞섭니다."
  InvalidDraft err -> describeError err
  FinalOwnerMissing -> "Final Owner가 존재하지 않습니다."
  UnknownOwner uid -> "책임자 " <> unUserId uid <> "이(가) 구성원 명단에 없습니다."
  AuthorityMissing gid description -> "목표 " <> showGoal gid description <> "의 책임자이지만 권한 기록이 전혀 없습니다."
  AuthorityInsufficient {} -> "책임에 비해 권한이 부족합니다."
  SharedMetricOwnership _ -> "두 명 이상이 동일한 결과를 최종 책임지고 있습니다."
  OwnerOverloaded count -> "한 사람이 " <> T.pack (show count) <> "개의 목표를 최종 책임지고 있습니다."
  DecisionConcentration uid share -> unUserId uid <> "이(가) 전체 조직 의사결정 권한의 " <> pct share <> "를 가지고 있습니다."
  ReviewWithoutOutcome _ -> "This review produced no decision."
  ActiveGoalWithoutResult -> "활성화된 뒤 보고된 결과가 없습니다."
  GoalPastDeadline -> "마감이 지났지만 목표가 달성되지 않았습니다."

renderDiagnosticDetails :: DiagnosticMessage -> [Text]
renderDiagnosticDetails = \case
  AuthorityInsufficient description target unit coverage missing available required ->
    [ "Responsibility: " <> description <> " = " <> T.pack (show target) <> " " <> unit
    , "Controls " <> pct coverage <> " of required resources"
    ]
      ++ [T.pack (show permission) <> " = False" | permission <- missing]
      ++ [ "Budget = "
             <> T.pack (show (unMoney available))
             <> " (required "
             <> T.pack (show (unMoney required))
             <> ")"
         | available < required
         ]
  SharedMetricOwnership owners -> [unGoalId gid <> " -> " <> unUserId uid | (gid, uid) <- owners]
  DecisionConcentration {} -> ["Possible bottleneck detected."]
  ReviewWithoutOutcome gid -> ["Goal: " <> unGoalId gid]
  _ -> []

showGoal :: GoalId -> Text -> Text
showGoal gid description = unGoalId gid <> " \"" <> description <> "\""

pct :: Double -> Text
pct x = T.pack (show (round (x * 100) :: Int)) <> "%"

renderDiagnostic :: Diagnostic -> Text
renderDiagnostic diagnostic =
  let view = presentDiagnostic diagnostic
   in T.unlines
        ( [ T.toUpper (T.pack (show (severity view))) <> " " <> code view
          , subject view
          , message view
          ]
            ++ map ("  " <>) (details view)
        )
