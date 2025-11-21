module Lib
    ( someFunc
    ) where

import           Control.Exception.Safe (displayException, tryAny)

import           Data.Foldable          (fold)
import           Data.Kind              (Type)

import           System.Directory       (getPermissions, writable)
import           System.Environment     (getEnv)
import           System.IO              (hPutStr, stderr, stdout)

-- |
--
type Level :: Type
data Level = Info | Error

-- |
--
type Event :: Type
data Event = Event Level String

-- |
--
type Log :: Type
data Log = Log { record :: Event -> IO ()
               }

-- |
--
instance Semigroup Log where
  (<>) = multiLog

-- |
--
instance Monoid Log where
  mempty = nullLog

-- |
--
consoleLog :: Log
consoleLog = Log $ \(Event level message) -> hPutStr (standardStream level) (message <> "\n")
  where
    standardStream Info  = stdout
    standardStream Error = stderr

-- |
--
fileLog :: (Level -> FilePath) -> Log
fileLog path = Log $ \(Event level message) -> appendFile (path level) (message <> "\n")

-- |
--
nullLog :: Log
nullLog = Log $ \_ -> return ()

-- |
--
formattedLog :: String -> Log -> Log
formattedLog topic log = Log $ \event -> record log (formatEvent topic event)

-- |
--
formatEvent :: String -> Event -> Event
formatEvent topic (Event level msg) = Event level msg'
  where
    msg' = paren (topic ! levelString level) ! msg

-- |
--
paren :: String -> String
paren x = "(" <> x <> ")"

-- |
--
(!) :: String -> String -> String
x ! y = x <> " " <> y

-- |
--
levelString :: Level -> String
levelString Info  = "info"
levelString Error = "error"

-- |
--
multiLog :: Log -> Log -> Log
multiLog log1 log2 = Log $ \event -> do
  record log1 event
  record log2 event

-- |
--
recoverFromException :: Log -> IO a -> IO (Maybe a)
recoverFromException log action = do
  result <- tryAny action
  case result of
    Left e -> do
      record log (Event Error (displayException e))
      return Nothing
    Right x -> do
      return (Just x)

-- |
--
someFunc :: IO ()
someFunc = do
  let bootLog = formattedLog "Boot" consoleLog
  record bootLog (Event Info "Starting")

  fileLog <- recoverFromException bootLog initFileLog
  let appLog = formattedLog "App" consoleLog <> fold fileLog
  record appLog (Event Info "Application started")

-- |
--
initFileLog :: IO Log
initFileLog = do
  infoPath <- envLogPath "INFO"
  errorPath <- envLogPath "ERROR"
  let path Info  = infoPath
      path Error = errorPath
  return (fileLog path)

-- |
--
envLogPath :: String -> IO String
envLogPath varName = do
  path <- getEnv varName
  assertWritable path
  return path

-- |
--
assertWritable :: FilePath -> IO ()
assertWritable path = do
  permissions <- getPermissions path
  case writable permissions of
    True  -> return ()
    False -> fail ("Log path" ! path ! "is not wriable")
