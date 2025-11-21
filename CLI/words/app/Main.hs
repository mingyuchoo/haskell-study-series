module Main
    where

import           Data          (grid, languages)

import           Flow          ((<|))

import           Lib           (Game, completed, fillInBlanks, formatGame,
                                makeGame, playGame)

import           System.IO     (BufferMode (NoBuffering), hSetBuffering, stdout)
import           System.Random (newStdGen)

-- |
--
main :: IO ()
main = do
  gen <- newStdGen
  let filledInGrid = fillInBlanks gen grid
      game = makeGame filledInGrid languages
  hSetBuffering stdout NoBuffering
  playTurn game


-- |
--
playTurn :: Game -> IO ()
playTurn game = do
  putStrLn . formatGame <| game
  putStr "Please enter a word> "
  word <- getLine

  let newGame = playGame game word
  if completed newGame
  then putStrLn "Congratulations!"
  else playTurn newGame
