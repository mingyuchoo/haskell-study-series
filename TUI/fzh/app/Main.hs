{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import Lib
import System.IO
    ( BufferMode (NoBuffering)
    , hSetBuffering
    , stdout
    )
import System.Environment (getArgs)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of
    ["--version"] -> putStrLn "fzh 0.1.0 - Fuzzy Finder in Haskell"
    ["--help"] -> printHelp
    _ -> tuiMain

printHelp :: IO ()
printHelp = putStrLn $ unlines
    [ "fzh - Fuzzy Finder in Haskell"
    , ""
    , "Usage: command | fzh"
    , ""
    , "Key bindings:"
    , "  Type to filter"
    , "  ↑/↓     Navigate results"
    , "  Enter   Select and exit"
    , "  Esc     Exit without selection"
    , "  Ctrl-C  Exit"
    , ""
    , "Options:"
    , "  --version  Show version"
    , "  --help     Show this help"
    ]
