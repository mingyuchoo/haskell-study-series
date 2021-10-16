{-# OPTIONS_GHC -fwarn-missing-signatures #-}

{-# LANGUAGE ExplicitForAll           #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Lib
    ( someFunc
    ) where

import           Control.Exception  (bracketOnError)
import           Control.Monad      (mapM, mapM_, (>>))
import           Data.List
    ( delete
    , head
    , lines
    , lookup
    , tail
    , unlines
    , zipWith
    )
import           Prelude
    ( IO (..)
    , Maybe (..)
    , String (..)
    , read
    , return
    , show
    , (!!)
    , ($)
    , (++)
    )
import           System.Directory   (removeFile, renameFile)
import           System.Environment (getArgs, getProgName)
import           System.IO
    ( IOMode (..)
    , appendFile
    , getLine
    , hClose
    , hGetContents
    , hPutStr
    , openFile
    , openTempFile
    , putStr
    , putStrLn
    , readFile
    )

-- | application
someFunc :: IO ()
someFunc = do
    argList <- getArgs
    case argList of
        [] -> putStrLn usage
        _  -> action args
          where
            command = head argList
            args = tail argList
            (Just action) = lookup command dispatch

usage :: String
usage = "Usage: stack run COMMAND FILENAME [ITEM]\n" ++
        "  COMMAND - add \"todo item\"\n" ++
        "          - view\n" ++
        "          - remove\n" ++
        " FILENAME - todo.txt"


dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
           , ("view", view)
           , ("remove", remove)
           ]

-- | add todo item
add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = putStrLn "The add command takes exactly two arguments."


-- | view todo item
view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
    putStr $ unlines numberedTasks
view _ = putStrLn "The view command takes exactly one argument."

-- | remove todo item
remove :: [String] -> IO ()
remove [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
    putStr $ unlines numberedTasks
    putStrLn "These are your TODO items:"
    mapM_ putStrLn numberedTasks
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "." "todo.temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)
remove _ = putStrLn "The remove command takes exactly two arguments."

