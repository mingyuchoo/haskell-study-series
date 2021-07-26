{-# LANGUAGE UnicodeSyntax #-}

module Main
    where

import           Chapter2.SimpleFunctions

main ∷ IO ()
main = do
  putStrLn (firstOrEmpty [])
  putStrLn (firstOrEmpty ["hello", "hola"])

  print ([1,2] +++ [3,4])
