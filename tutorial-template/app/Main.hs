{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
module Main where

-- | Count spaces
--
-- >>> countSpace ""
-- 0
-- >>> countSpace "abracadabra"
-- 0
-- >>> countSpace "Hello, World!"
-- 1
-- >>> countSpace "    "
-- 4
--
-- prop> countSpace s == sum [ 1 | c <- s, c == ' ' ]
--
countSpace :: String -> Int
countSpace = length . filter (' ' ==)

main :: IO ()
main = putStrLn "Hello, Haskell!"
