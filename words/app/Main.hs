module Main
    where

import           Data
import           Flow
import           Lib
import           System.IO

main :: IO ()
main = do
  let game = makeGame grid languages
  hSetBuffering stdout NoBuffering
  playTurn game


playTurn :: Game -> IO ()
playTurn game = do
  putStrLn . formatGame <| game
  putStr "Please enter a word> "
  word <- getLine

  let newGame = playGame game word
  if completed newGame
  then putStrLn "Congratulations!"
  else playTurn newGame
