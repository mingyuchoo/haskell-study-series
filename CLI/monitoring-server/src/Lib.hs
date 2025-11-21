{-# LANGUAGE NumericUnderscores #-}

module Lib
    where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception.Safe
import           Control.Monad             (forever, when)

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8
import           Data.Foldable             (asum, find, for_)
import           Data.Kind                 (Type)
import           Data.Maybe                (mapMaybe)
import           Data.Ratio                (Ratio, (%))
import qualified Data.Sequence             as Seq

import qualified Network.Socket            as S
import           Network.Socket.ByteString (recv, sendAll)

import           System.Environment
import           System.Exit
import           System.Signal

-- |
--
type EventReport :: Type
data EventReport = Success | Failure
     deriving (Eq)

-- |
--
type SystemStatus :: Type
data SystemStatus = Okay | Alarm
     deriving (Eq)

-- |
--
someFunc :: IO ()
someFunc = do
  args <- getArgs
  case args of ["aggregate-reports"] -> aggregateReportsMain -- 1
               ["send-demo-reports"] -> sendDemoReportsMain  -- 2
               _                     -> die "Invalid args"

-- |
--
aggregateReportsMain :: IO ()
aggregateReportsMain =
  withServerSocket $ \serverSocket -> do
  putStrLn "The monitoring server has started."
  reportQueue <- atomically newTQueue
  alarmQueue  <- atomically newTQueue
  foldr1 race_  [ receiveReports serverSocket reportQueue -- 1
                , analyzeReports reportQueue alarmQueue   -- 2
                , sendAlarms alarmQueue                   -- 3
                , waitForTerminationSignal
                ]
  putStrLn "The monitoring server is stopping."

-- |
--
waitForTerminationSignal :: IO ()
waitForTerminationSignal = do
  terminate <- atomically $ newTVar False
  installHandler sigTERM $ \_signal -> atomically (writeTVar terminate True)
  atomically $ readTVar terminate >>= check

-- |
--
encodeReport :: EventReport -> Char
encodeReport r = case r of Failure -> '0'
                           Success -> '1'

-- |
--
decodeReport :: Char -> Maybe EventReport
decodeReport c = find (\r -> encodeReport r == c) [Failure, Success]

-- |
--
serverAddress :: S.SockAddr
serverAddress = S.SockAddrUnix "\0phrasebook/monitoring"

-- |
--
openSocket :: IO S.Socket
openSocket = S.socket S.AF_UNIX S.Stream S.defaultProtocol

-- |
--
withServerSocket :: (S.Socket -> IO c) -> IO c
withServerSocket action =
  bracket openSocket S.close $ \serverSocket -> do
  S.bind serverSocket serverAddress       -- 1
  S.listen serverSocket S.maxListenQueue  -- 2
  action serverSocket

-- |
--
receiveReports :: S.Socket -> TQueue EventReport -> IO b
receiveReports serverSocket reportQueue =
  forever $ mask $ \unmask -> do
  (clientSocket, _clientAddr) <- S.accept serverSocket
  forkFinally
    (unmask (receiveReports' clientSocket reportQueue))
    (\_ -> S.close clientSocket)

-- |
--
receiveReports' :: S.Socket -> TQueue EventReport -> IO ()
receiveReports' clientSocket reportQueue = continue
  where
    continue = do
      receivedBytes <- recv clientSocket 1024
      case BS.length receivedBytes of 0 -> return ()
                                      _ -> receiveReports'' receivedBytes reportQueue >> continue

-- |
--
receiveReports'' :: Data.ByteString.Char8.ByteString -> TQueue EventReport -> IO ()
receiveReports'' receivedBytes reportQueue =
  for_ @[] (Data.ByteString.Char8.unpack receivedBytes) $ \c ->
  for_ @Maybe (decodeReport c) $ \r ->
  atomically (writeTQueue reportQueue r)

-- |
--
reportWindowSize :: Int
reportWindowSize = 10

-- |
--
okayThreshold :: Ratio Int
okayThreshold = 80 % 100

-- |
--
alarmThreshold :: Ratio Int
alarmThreshold = 50 % 100

-- |
--
analysis :: Seq.Seq EventReport -> Maybe SystemStatus
analysis reports  | Seq.length reports < reportWindowSize = Nothing
                  | successRate <= alarmThreshold         = Just Alarm
                  | successRate >= okayThreshold          = Just Okay
                  | otherwise                             = Nothing
  where
    successes   = Seq.filter (== Success) reports
    successRate = Seq.length successes % Seq.length reports

-- |
--
analyzeReports :: TQueue EventReport -> TQueue SystemStatus -> IO b
analyzeReports reportQueue alarmQueue = continue Nothing Seq.empty
  where
    continue status reports = do
      newReport <- atomically (readTQueue reportQueue)
      let
        reports' = Seq.take reportWindowSize (newReport Seq.<| reports)
        status'  = asum [analysis reports', status]
      for_ @Maybe status' $ \s -> when (status /= status') $ atomically (writeTQueue alarmQueue s)
      continue status' reports

-- |
--
sendAlarms :: TQueue SystemStatus -> IO b
sendAlarms alarmQueue = forever $ do
  a <- atomically (readTQueue alarmQueue)
  case a of Alarm -> putStrLn "Alarm! System is in a degraded state."
            Okay  -> putStrLn "System status is normal."

-- |
--
sendDemoReportsMain :: IO ()
sendDemoReportsMain = do
  reportQueue <- atomically newTQueue
  foldr1 race_ [ generateReports reportQueue
               , sendReports reportQueue
               ]

-- |
--
demoReports :: [EventReport]
demoReports =  mapMaybe decodeReport "1111111111111010011000001000000100011101111110111111"
                                   --  successes          failures            successes

-- |
--
generateReports :: TQueue EventReport -> IO ()
generateReports reportQueue = for_ demoReports $ \r -> do
  atomically (writeTQueue reportQueue r)
  threadDelay 100_000

-- |
--
withClientSocket :: (S.Socket -> IO c) -> IO c
withClientSocket action = bracket openSocket S.close $ \clientSocket -> do
  S.connect clientSocket serverAddress
  action clientSocket

-- |
--
sendReports :: TQueue EventReport -> IO c
sendReports reportQueue = withClientSocket $ \clientSocket -> forever $ do
  r <- atomically (readTQueue reportQueue)
  putStrLn (case r of Success -> "1 (success)"
                      Failure -> "0 (failure)")
  sendAll clientSocket (Data.ByteString.Char8.pack [encodeReport r])
