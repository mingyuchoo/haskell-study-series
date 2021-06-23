{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}

module Main( split
           , main) where

split :: String -> [String]
split [] = [""]
split (x:xs)
  | x == ':' = "" : rest
  | otherwise = (x : head rest) : tail rest
  where
    rest = split xs


hello :: [String] -> Int
hello []          = error "No head for empty lists!"
hello [""]        = error "No empty string!"
hello (x : y : _) = 60 * (read x)  + (read y) :: Int


main :: IO ()
main = putStrLn "Hello, World"
