module Lib
    ( someFunc
    ) where

import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.STM.TVar
import           Control.Monad               (forever)
import           Control.Monad.STM

import           Data.Foldable               (asum, forM_, for_)
import           Data.Kind                   (Type)
import qualified Data.Sequence               as Seq
import           Data.Traversable            (for)

import           System.Random.MWC           (createSystemRandom, uniformR)

-- |
--
someFunc :: IO ()
someFunc = do
  accountList <- for [1 .. 10] $ \_ -> atomically $ newTVar (100 :: Integer)
  let
    accountSeq = Seq.fromList accountList
    randomAccount rng = uniformR (1, Seq.length accountSeq) rng >>= \i ->
      return $ Seq.index accountSeq (i - 1)


  for_ [1 .. 500] $ \_ -> forkIO $ do
    rng <- createSystemRandom
    forever $ do
      d <- uniformR (10, 50) rng
      threadDelay d

      sender <- randomAccount rng
      recipient <- randomAccount rng

      amount <- uniformR (1, 10) rng >>= \x -> return $ toInteger (x :: Int)


      atomically $ asum
        [ modifyTVar' sender (\x -> x - amount) >>= \_ ->
            readTVar sender >>= \x ->
            check (x >= 0) >>= \_ ->
            modifyTVar' recipient (\x -> x + amount)
        , return ()
        ]

  for_ [1 .. 4] $ \_ ->
    threadDelay 500000 >>= \_ ->
    atomically (for accountList readTVar) >>= \balances ->
    print balances >>= \_ ->
    print ("Total: " <> show (sum balances))
