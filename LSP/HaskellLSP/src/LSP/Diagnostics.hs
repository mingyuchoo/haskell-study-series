{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Diagnostics engine for syntax error detection and reporting
module LSP.Diagnostics
    ( -- * Diagnostic Types
      DiagnosticInfo (..)
    , DiagnosticSeverity (..)
      -- * Syntax Error Detection
    , analyzeDiagnostics
    , detectSyntaxErrors
      -- * LSP Conversion
    , publishDiagnostics
    , toLspDiagnostic
      -- * Error Classification
    , classifySyntaxError
    ) where

import           Analysis.Parser               (ParseError (..),
                                                ParsedModule (..), parseModule)

import           Data.Text                     (Text)
import qualified Data.Text                     as T

import           GHC.Generics                  (Generic)

import           Language.LSP.Protocol.Message (SMethod (SMethod_TextDocumentPublishDiagnostics))
import           Language.LSP.Protocol.Types   (Diagnostic (..),
                                                DiagnosticSeverity (..),
                                                Position (..),
                                                PublishDiagnosticsParams (..),
                                                Range (..), Uri)
import           Language.LSP.Server           (LspM, sendNotification)

-- | Internal diagnostic information
data DiagnosticInfo = DiagnosticInfo { diagRange    :: Range
                                     , diagSeverity :: DiagnosticSeverity
                                     , diagMessage  :: Text
                                     , diagCode     :: Maybe Text
                                     , diagSource   :: Text
                                     }
     deriving (Eq, Generic, Show)

-- | Analyze document and produce diagnostics
-- This function detects syntax errors and other issues in Haskell source code
analyzeDiagnostics :: ParsedModule -> [DiagnosticInfo]
analyzeDiagnostics parsedModule =
  let sourceText = pmSource parsedModule
      syntaxErrors = detectSyntaxErrors sourceText
      -- Future: Add other diagnostic types (type errors, warnings, etc.)
  in syntaxErrors

-- | Detect syntax errors in Haskell source code
-- Returns a list of diagnostic information for syntax errors with positions
detectSyntaxErrors :: Text -> [DiagnosticInfo]
detectSyntaxErrors sourceText =
  case parseModule sourceText of
    Left parseError -> [parseErrorToDiagnostic parseError]
    Right _parsedModule ->
      -- If parsing succeeded, check for other syntax issues
      let linesOfCode = T.lines sourceText
          numberedLines = zip [0..] linesOfCode
          syntaxIssues = concatMap checkLineSyntax numberedLines
      in syntaxIssues

-- | Convert ParseError to DiagnosticInfo
parseErrorToDiagnostic :: ParseError -> DiagnosticInfo
parseErrorToDiagnostic parseError =
  let range = case parseErrorRange parseError of
        Just r  -> r
        Nothing -> Range (Position 0 0) (Position 0 1) -- Default to first character
      severity = DiagnosticSeverity_Error
      message = parseErrorMessage parseError
      code = Just "parse-error"
      source = "haskell-lsp"
  in DiagnosticInfo range severity message code source

-- | Check individual line for syntax issues
checkLineSyntax :: (Int, Text) -> [DiagnosticInfo]
checkLineSyntax (lineNum, line) =
  checkUnmatchedParens lineNum line
  <> checkInvalidChars lineNum line
  <> checkIncompleteStrings lineNum line

-- | Check for unmatched parentheses
checkUnmatchedParens :: Int -> Text -> [DiagnosticInfo]
checkUnmatchedParens lineNum line =
  let openParens = T.count "(" line
      closeParens = T.count ")" line
      openBrackets = T.count "[" line
      closeBrackets = T.count "]" line
      openBraces = T.count "{" line
      closeBraces = T.count "}" line

      parenIssues = if openParens > closeParens
                   then [createDiagnostic lineNum line "Unmatched opening parenthesis" "unmatched-paren"]
                   else if closeParens > openParens
                   then [createDiagnostic lineNum line "Unmatched closing parenthesis" "unmatched-paren"]
                   else []

      bracketIssues = if openBrackets > closeBrackets
                     then [createDiagnostic lineNum line "Unmatched opening bracket" "unmatched-bracket"]
                     else if closeBrackets > openBrackets
                     then [createDiagnostic lineNum line "Unmatched closing bracket" "unmatched-bracket"]
                     else []

      braceIssues = if openBraces > closeBraces
                   then [createDiagnostic lineNum line "Unmatched opening brace" "unmatched-brace"]
                   else if closeBraces > openBraces
                   then [createDiagnostic lineNum line "Unmatched closing brace" "unmatched-brace"]
                   else []
  in parenIssues <> bracketIssues <> braceIssues

-- | Check for invalid characters
checkInvalidChars :: Int -> Text -> [DiagnosticInfo]
checkInvalidChars lineNum line =
  let invalidChars = T.filter (\c -> c `elem` ("\0\1\2\3\4\5\6\7\8\11\12\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31" :: String)) line
  in if T.null invalidChars
     then []
     else [createDiagnostic lineNum line "Invalid control characters found" "invalid-chars"]

-- | Check for incomplete strings
checkIncompleteStrings :: Int -> Text -> [DiagnosticInfo]
checkIncompleteStrings lineNum line =
  let quoteCount = T.count "\"" line
      -- Simple check: odd number of quotes suggests incomplete string
  in if odd quoteCount && not (T.isSuffixOf "\\" line)
     then [createDiagnostic lineNum line "Incomplete string literal" "incomplete-string"]
     else []

-- | Create a diagnostic for a specific line
createDiagnostic :: Int -> Text -> Text -> Text -> DiagnosticInfo
createDiagnostic lineNum line message code =
  let range = Range
        (Position (fromIntegral lineNum) 0)
        (Position (fromIntegral lineNum) (fromIntegral $ T.length line))
      severity = DiagnosticSeverity_Error
      source = "haskell-lsp"
  in DiagnosticInfo range severity message (Just code) source

-- | Classify syntax error type for better error reporting
classifySyntaxError :: Text -> Text
classifySyntaxError errorMsg
  | "parse error" `T.isInfixOf` T.toLower errorMsg = "parse-error"
  | "unexpected" `T.isInfixOf` T.toLower errorMsg = "unexpected-token"
  | "missing" `T.isInfixOf` T.toLower errorMsg = "missing-token"
  | "unmatched" `T.isInfixOf` T.toLower errorMsg = "unmatched-delimiter"
  | otherwise = "syntax-error"

-- | Convert internal DiagnosticInfo to LSP Diagnostic
toLspDiagnostic :: DiagnosticInfo -> Diagnostic
toLspDiagnostic diagInfo = Diagnostic
  { _range = diagRange diagInfo
  , _severity = Just (diagSeverity diagInfo)
  , _code = Nothing  -- For now, skip the code field to avoid type issues
  , _codeDescription = Nothing
  , _source = Just (diagSource diagInfo)
  , _message = diagMessage diagInfo
  , _tags = Nothing
  , _relatedInformation = Nothing
  , _data_ = Nothing
  }

-- | Publish diagnostics to LSP client
publishDiagnostics :: Uri -> [DiagnosticInfo] -> LspM config ()
publishDiagnostics uri diagnosticInfos = do
  let lspDiagnostics = map toLspDiagnostic diagnosticInfos
      params = PublishDiagnosticsParams
        { _uri = uri
        , _version = Nothing
        , _diagnostics = lspDiagnostics
        }
  sendNotification SMethod_TextDocumentPublishDiagnostics params
