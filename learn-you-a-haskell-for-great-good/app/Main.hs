module Main where

import Chapter02.StartingOut
import Chapter03.TypesAndTypeclasses
import Chapter04.PatternMatching
import Chapter04.Guards


-- | main
main :: IO ()
main = do 
    putStrLn (show (doubleMe 3))
    putStrLn "Hello, Haskell!"
    putStrLn (removeNonUppercase "Hello Haskell!")
    putStrLn (lucky 7)
    putStrLn (sayMe 1)
    putStrLn (show (factorial 50))
    putStrLn (charName 'a')
    putStrLn (bmiTell 90 2.83)
