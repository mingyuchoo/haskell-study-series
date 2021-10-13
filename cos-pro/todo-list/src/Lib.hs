{-# LANGUAGE NoImplicitPrelude #-}

module Lib
    ( someFunc
    ) where

import           Control.Exception  (bracketOnError)
import           Control.Monad      (mapM, mapM_, (>>))
import           Data.List          (delete, lines, unlines, zipWith)
import           Prelude
    ( IO (..)
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
    ( appendFile
    , getLine
    , hClose
    , hPutStr
    , openTempFile
    , putStr
    , putStrLn
    , readFile
    )

-- | application
someFunc :: IO ()
someFunc = do
    args <- getArgs
    progName <- getProgName
    putStrLn "------------------------------"
    putStr "The program name is: " >> putStrLn progName
    putStr "The arguments are: " >> mapM putStrLn args
    putStrLn "\n------------------------------"

    -- addItem
    removeItem

-- | add todo item
addItem :: IO ()
addItem = do
    item <- getLine
    appendFile "todo.txt" (item ++ "\n")

-- | remove todo item
removeItem :: IO ()
removeItem = do
    contents <- readFile "todo.txt"
    let todoTasks = lines contents
        numberedTasks = zipWith
                            (\n line -> show n ++ " - " ++ line)
                            [0 ..]
                            todoTasks
    putStrLn "These are your To-Do items:"
    mapM_ putStrLn numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError
        (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile "todo.txt"
            renameFile tempName "todo.txt")


