module Lib
    ( cliMain
    ) where

import LSP.Server (runLspServer)
import System.Exit (exitWith, ExitCode(..))

cliMain :: IO ()
cliMain = do
  exitCode <- runLspServer
  exitWith (ExitFailure exitCode)
