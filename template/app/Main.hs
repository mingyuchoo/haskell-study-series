module Main
    where

import           Lib
import           System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  someFunc


-- | loop function using ((>>=))
loop1 :: IO ()
loop1 =
  putStr "A" >>= (\_ ->
  putStr "B" >>= (\_ ->
  putStr "C" >>= (\_ ->
  putStr "D" >>= (\_ ->
  putStrLn "E" >>= (\_ ->
  putStr "If you want to continue, answer \"yes\": " >>= (\_ ->
  getLine >>= (\x ->
  if x == "yes" then loop1 else return ())))))))


-- | loop function  using ((>>))
loop2 :: IO ()
loop2 =
  putStr "A" >>
  putStr "B" >>
  putStr "C" >>
  putStr "D" >>
  putStrLn "E" >>
  putStr "If you want to continue, answer \"yes\": " >>
  getLine >>= (\x -> if x == "yes" then loop1 else return ())


-- | loop function  using (do) notation
loop3 :: IO ()
loop3 = do
  putStr "A"
  putStr "B"
  putStr "C"
  putStr "D"
  putStrLn "E"
  putStr "If you want to continue, answer \"yes\": "
  x <- getLine
  if x == "yes" then loop1 else return ()

