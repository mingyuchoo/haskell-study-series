-- | Explicit Diagnostic JSON contract. Keys and tags are stable.
module MyOrg.Http.Codec.Diagnostic
  ( diagnosticViewCodec
  , compileReportViewCodec
  , severityCodec
  ) where

import Data.Aeson (Value (String), withObject, withText)
import Data.Text qualified as T
import MyOrg.Domain.Compiler
import MyOrg.Presentation.Diagnostic qualified as Diagnostic
import MyOrg.Serialization.Codec

diagnosticViewCodec :: Codec Diagnostic.DiagnosticView
diagnosticViewCodec = Codec encode decode
  where
    encode Diagnostic.DiagnosticView {..} =
      record
        [ ("code", Just (encodeValue textCodec code))
        , ("severity", Just (encodeValue severityCodec severity))
        , ("subject", Just (encodeValue textCodec subject))
        , ("message", Just (encodeValue textCodec message))
        , ("details", Just (encodeValue (listCodec textCodec) details))
        ]
    decode = withObject "DiagnosticView" $ \obj ->
      Diagnostic.DiagnosticView
        <$> field textCodec obj "code"
        <*> field severityCodec obj "severity"
        <*> field textCodec obj "subject"
        <*> field textCodec obj "message"
        <*> field (listCodec textCodec) obj "details"

compileReportViewCodec :: Codec Diagnostic.CompileReportView
compileReportViewCodec = Codec encode decode
  where
    encode Diagnostic.CompileReportView {..} =
      record
        [ ("errors", Just (encodeValue intCodec errors))
        , ("warnings", Just (encodeValue intCodec warnings))
        , ("infos", Just (encodeValue intCodec infos))
        , ("diagnostics", Just (encodeValue (listCodec diagnosticViewCodec) diagnostics))
        ]
    decode = withObject "CompileReportView" $ \obj ->
      Diagnostic.CompileReportView
        <$> field intCodec obj "errors"
        <*> field intCodec obj "warnings"
        <*> field intCodec obj "infos"
        <*> field (listCodec diagnosticViewCodec) obj "diagnostics"

severityCodec :: Codec Severity
severityCodec = Codec encode decode
  where
    encode = \case
      Error -> String "Error"
      Warning -> String "Warning"
      Info -> String "Info"
    decode = withText "Severity" $ \tag -> case tag of
      "Error"   -> pure Error
      "Warning" -> pure Warning
      "Info"    -> pure Info
      _         -> fail ("Unknown Severity: " <> T.unpack tag)
