{-# LANGUAGE StandaloneKindSignatures #-}

module Chapter09.InputAndOutput
    where

import           Control.Monad      (forM, forever, when)
import           Data.Char
import           System.Environment
import           System.IO
import           System.IO.Error

--------------------------------------------------------------------------------
-- | make factorial number
factorial :: Int -> Int
factorial n | n == 0 = 0
            | n == 1 = 1
            | otherwise = n * factorial (n - 1)

--------------------------------------------------------------------------------
-- | use case of `getLine`
askName :: IO ()
askName = do
    _ <- putStrLn "What's your first name?"
    firstName <- getLine
    _ <- putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName  = map toUpper lastName
    _ <- putStrLn $
        "Hey " ++ bigFirstName ++
        " "    ++ bigLastName  ++
        ", how are you?"
    return ()

--------------------------------------------------------------------------------
-- |
makeReverse :: IO ()
makeReverse = do
    line <- getLine
    if null line
        then return ()
        else (putStrLn $ reverseWords line) >>
             makeReverse >>
             return ()

-- |
reverseWords :: String -> String
reverseWords = unwords . map reverse . words

--------------------------------------------------------------------------------
-- | use cases of `return`
assertReturn :: IO ()
assertReturn = do
    _ <- return ()
    a <- return "HAHAHA" -- :: Monad m => m [Char]
    line <- getLine
    b <- return "BLAH BLAH BLAH" -- :: Monad m => m [Char]
    _ <- return 4
    _ <- putStrLn $ a ++ " " ++ b
    _ <- putStrLn line
    let c = "hell"
        d = "yeah"
    _ <- putStrLn $ c ++ " " ++ d
    return ()

--------------------------------------------------------------------------------
-- | example of `when`
exampleWhen :: IO ()
exampleWhen = do
    input <- getLine
    -- when expression
    when (input == "SWORDFISH") $ do
        _ <- putStrLn input
        return ()
    -- if expression
    if (input == "SWORDFISH")
        then putStrLn input
        else return ()

--------------------------------------------------------------------------------
-- | example of `sequence`
--  sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
exampleSequence :: IO ()
exampleSequence = do
    -- sequence expression
    rs <- sequence [getLine, getLine, getLine]
    print rs
    -- bind statement
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]

--------------------------------------------------------------------------------
-- | example of `mapM` and `mapM_`
exampleMapM :: IO ()
exampleMapM = do
    _ <- sequence $ map print [1,2,3,4,5]
    _ <- mapM  print [1,2,3,4,5]
    _ <- mapM_ print [1,2,3,4,5]
    return ()


--------------------------------------------------------------------------------
-- | example of `forever`
exampleForever :: IO ()
exampleForever = do
    forever $ do
        putStr "Give me some input: "
        input <- getLine
        putStrLn $ map toUpper input

--------------------------------------------------------------------------------
-- | example of `forM`
exampleForM :: IO ()
exampleForM = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3, and 4 are: "
    mapM putStrLn colors
    return ()

--------------------------------------------------------------------------------
-- | evaludate functions
someFunc :: IO ()
someFunc = do
    putStrLn "----------------------------"
    -- askName
    -- makeReverse
    -- assertReturn
    -- exampleWhen
    -- exampleSequence
    -- exampleMapM
    -- exampleForever
    -- exampleForM



--------------------------------------------------------------------------------
{-- input redirection --}
----- if you want to quit, type Ctr+d
-- import Control.Monad
-- import Data.Char
-- main' = forever $ do
--     putStr "Give me some input: "
--     l <- getLine
--     putStrLn $ map toUpper l
--------------------------------------------------------------------------------
-- import Data.Char
-- main' = do
--     contents <- getContents
--     putStr $ map toUpper contents     -- ==  putStr (map toUpper contents)
--------------------------------------------------------------------------------
-- main' = do
--     contents <- getContents
--     putStr (shortLinesOnly contents)
--
-- shortLinesOnly :: String -> String
-- shortLinesOnly input =
--     let allLines   = lines input
--         shortLines = filter (\line -> length line < 10) allLines
--         result     = unlines shortLines
--     in  result
--------------------------------------------------------------------------------
-- main' = interact shortLinesOnly
-- shortLinesOnly :: String -> String
-- shortLinesOnly input =
--     let allLines   = lines input
--         shortLines = filter (\line -> length line < 10) allLines
--         result     = unlines shortLines
--     in  result
--------------------------------------------------------------------------------
-- main' = interact $ unlines . filter ((<10) . length) . lines
--------------------------------------------------------------------------------
-- main' = interact respondPalindromes
-- respondPalindromes contents = unlines (map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") (lines contents))
--     where isPalindrome xs = xs == reverse xs
-- main' = interact respondPalindromes
-- respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines
--     where isPalindrome xs = xs == reverse xs
--------------------------------------------------------------------------------
{-- // girlfriend.txt

Hey! Hey! You! You!
I don't like your girlfriend!
No way! No way!
I think you need a new one!

--}
-- import System.IO
-- main' = do
--     handle <- openFile "girlfriend.txt" ReadMode
--     contents <- hGetContents handle
--     putStr contents
--     hClose handle
--------------------------------------------------------------------------------
-- import System.IO
-- main' = do
--     withFile "girlfriend.txt" ReadMode (\handle -> do
--         contents <- hGetContents handle
--         putStr contents)
--------------------------------------------------------------------------------
-- import System.IO
-- main' = do
--     withFile' "girlfriend.txt" ReadMode (\handle -> do
--         contents <- hGetContents handle
--         putStr contents)
--
-- withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
-- withFile' path mode f = do
--     handle <- openFile path mode
--     result <- f handle
--     hClose handle
--     return result
--------------------------------------------------------------------------------
-- import System.IO
-- main' = do
--     contents <- readFile "girlfriend.txt"
--     putStr contents
--------------------------------------------------------------------------------
-- import System.IO
-- import Data.Char
-- main' = do
--     contents <- readFile "girlfriend.txt"
--     writeFile "girlfriendcaps.txt" (map toUpper contents)
--------------------------------------------------------------------------------
-- import System.IO
-- main' = do
--     todoItem <- getLine
--     appendFile "todo.txt" (todoItem ++ "\n")
--------------------------------------------------------------------------------
-- import System.IO
-- main' = do
--     withFile "todo.txt" ReadMode (\handle -> do
--         contents <- hGetContents handle
--         putStr contents)
--------------------------------------------------------------------------------
-- import System.IO
-- main' = do
--     withFile "todo.txt" ReadMode (\handle -> do
--         hSetBuffering handle $ BlockBuffering (Just 2048)
--         contents <- hGetContents handle
--         putStr contents)
--------------------------------------------------------------------------------
-- import System.IO
-- import System.Directory
-- import Data.List
-- main' = do
--     handle <- openFile "todo.txt" ReadMode
--     (tempName, tempHandle) <- openTempFile "." "temp"
--     contents <- hGetContents handle
--     let todoTasks = lines contents
--         numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
--     putStrLn "These are your TO-DO items:"
--     putStr $ unlines numberedTasks
--     putStrLn "Which one do you want to delete?"
--     numberString <- getLine
--     let number = read numberString
--         newTodoItems = delete (todoTasks !! number) todoTasks
--     hPutStr tempHandle $ unlines newTodoItems
--     hClose handle
--     hClose tempHandle
--     removeFile "todo.txt"
--     renameFile tempName "todo.txt"
--------------------------------------------------------------------------------
{-- Command line arguments --}
-- import System.Environment
-- import Data.List
-- main' = do
--     args <- getArgs
--     progName <- getProgName
--     putStrLn "The arguments are:"
--     mapM putStrLn args
--     putStrLn "The program name is:"
--     putStrLn progName
--------------------------------------------------------------------------------
-- import Data.List
-- import System.Directory
-- import System.Environment
-- import System.IO
--
-- dispatch :: [(String, [String] -> IO ())]
-- dispatch = [("add", add), ("view", view), ("remove", remove)]
--
-- main' = do
--   (command:args) <- getArgs
--   let (Just action) = lookup command dispatch
--   action args
--
-- add :: [String] -> IO ()
-- add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
--
-- view :: [String] -> IO ()
-- view [fileName] = do
--   contents <- readFile fileName
--   let todoTasks = lines contents
--       numberedTasks =
--         zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
--   putStr $ unlines numberedTasks
--
-- remove :: [String] -> IO ()
-- remove [fileName, numberString] = do
--   handle <- openFile fileName ReadMode
--   (tempName, tempHandle) <- openTempFile "." "temp"
--   contents <- hGetContents handle
--   let number = read numberString
--       todoTasks = lines contents
--       newTodoItems = delete (todoTasks !! number) todoTasks
--   hPutStr tempHandle $ unlines newTodoItems
--   hClose handle
--   hClose tempHandle
--   removeFile fileName
--   renameFile tempName fileName
--------------------------------------------------------------------------------
-- randomNumber :: (Num a) => a
-- randomNumber = 4

--------------------------------------------------------------------------------
-- import System.Random
-- threeCoins :: StdGen -> (Bool, Bool, Bool)
-- threeCoins gen =
--   let (firstCoin,   newGen) = random gen
--       (secondCoin, newGen') = random newGen
--       (thirdCoin, newGen'') = random newGen'
--   in  (firstCoin, secondCoin, thirdCoin)
--------------------------------------------------------------------------------
-- import System.Random
-- randoms' :: (RandomGen g, Random a) => g -> [a]
-- randoms' gen =
--   let (value, newGen) = random gen
--   in   value:randoms' newGen
--------------------------------------------------------------------------------
-- import System.Random
-- main' = do
--   gen <- getStdGen
--   putStrLn $ take 20 (randomRs ('a','z') gen)
--   gen2 <- getStdGen
--   putStr   $ take 20 (randomRs ('a','z') gen2)
--------------------------------------------------------------------------------
-- import           Data.List
-- import           System.Random
-- main' = do
--   gen <- getStdGen
--   let randomChars     = randomRs ('a','z') gen
--       (first20, rest) = splitAt 20 randomChars
--       (second20,   _) = splitAt 20 rest
--   putStrLn first20
--   putStr   second20
--------------------------------------------------------------------------------
-- import           System.Random
-- main' = do
--     gen <- getStdGen
--     putStrLn $ take 20 (randomRs ('a','z') gen)
--     gen' <- newStdGen
--     putStr $ take 20 (randomRs ('a','z') gen')
--------------------------------------------------------------------------------
-- import           Control.Monad (when)
-- import           System.Random
--
-- main' = do
--     gen <- getStdGen
--     askForNumber gen
--
-- askForNumber :: StdGen -> IO ()
-- askForNumber gen = do
--     let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
--     putStr "Which number in the range from 1 to 10 am I thinking of? "
--     numberString <- getLine
--     when (not $ null numberString) $ do
--         let number = read numberString
--         if randNumber == number
--             then putStrLn "You are correct!"
--             else putStrLn $ "Sorry, it was " ++ show randNumber
--         askForNumber newGen
--------------------------------------------------------------------------------
-- import           Control.Monad (when)
-- import           System.Random
--
-- main' = do
--     gen <- getStdGen
--     let (randNumber, _) = randomR (1,10) gen :: (Int, StdGen)
--     putStr "Which number in the range from 1 to 10 am I thinking of? "
--     numberString <- getLine
--     when (not $ null numberString) $ do
--         let number = read numberString
--         if randNumber == number
--             then putStrLn "You are correct!"
--             else putStrLn $ "Sorry, it was " ++ show randNumber
--         newStdGen
--         main'
--------------------------------------------------------------------------------
{-- Bytestrings --}
-- import qualified Data.ByteString      as S
-- import qualified Data.ByteString.Lazy as B
-- import           System.Environment
--
-- main' = do
--     (fileName1:fileName2:_) <- getArgs
--     copyFile fileName1 fileName2
--
-- copyFile :: FilePath -> FilePath -> IO ()
-- copyFile source dest = do
--     contents <- B.readFile source
--     B.writeFile dest contents
--------------------------------------------------------------------------------
-- import           System.Environment
-- import           System.IO
-- main' = do
--     (fileName:_) <- getArgs
--     contents <- readFile fileName
--     putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
--------------------------------------------------------------------------------
-- import           System.Directory
-- import           System.Environment
-- import           System.IO
-- main' = do
--     (fileName:_) <- getArgs
--     fileExists <- doesFileExist fileName
--     if fileExists
--         then do
--             contents <- readFile fileName
--             putStrLn $ "The file has " ++ show (length (lines contents)) ++ "lines!"
--         else do
--             putStrLn "The file doesn't exist!"
--------------------------------------------------------------------------------
-- import           System.Environment
-- import           System.IO
-- import           System.IO.Error
-- main' = toTry `catchIOError` handler
-- toTry :: IO ()
-- toTry = do
--     (fileName:_) <- getArgs
--     contents <- readFile fileName
--     putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
-- handler :: IOError -> IO ()
-- handler e = putStrLn "Whoops, had some trouble!"
--------------------------------------------------------------------------------
-- import           System.Environment
-- import           System.IO
-- import           System.IO.Error
-- main' = toTry `catchIOError` handler
-- toTry :: IO ()
-- toTry = do
--     (fileName:_) <- getArgs
--     contents     <- readFile fileName
--     putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
--
-- handler :: IOError -> IO ()
-- handler e
--     | isDoesNotExistError e = putStrLn "The file doesn't exist!"
--     | otherwise = ioError e
--------------------------------------------------------------------------------
main' = toTry `catchIOError` handler

toTry :: IO ()
toTry = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e =
        case ioeGetFileName e of
            Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path
            Nothing   -> putStrLn "Whoops! File does not exist at unkown location"
    | otherwise = ioError e

--------------------------------------------------------------------------------

