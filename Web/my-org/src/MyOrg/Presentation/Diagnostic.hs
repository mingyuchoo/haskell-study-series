module MyOrg.Presentation.Diagnostic (renderDiagnostic, renderDiagnosticMessage) where

import Data.Text (Text)
import qualified Data.Text as T
import MyOrg.Domain.Compiler
import MyOrg.Presentation.Error (describeError)

renderDiagnosticMessage :: DiagnosticMessage -> Text
renderDiagnosticMessage (PlainMessage message) = message
renderDiagnosticMessage (InvalidDraft err) = describeError err

-- | 컴파일러 스타일의 텍스트 출력.
--
-- > ERROR O001
-- > Goal: Enterprise Revenue +30%
-- > Final Owner가 존재하지 않습니다.
renderDiagnostic :: Diagnostic -> Text
renderDiagnostic Diagnostic{..} =
  T.unlines
    ( [ T.toUpper (T.pack (show diagnosticSeverity)) <> " " <> diagnosticCode
      , diagnosticSubject
      , renderDiagnosticMessage diagnosticMessage
      ]
        ++ map ("  " <>) diagnosticDetails
    )

