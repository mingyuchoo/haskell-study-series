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

module Lib
    ( someFunc
    ) where

import           Data.List


-- "05:31" 331
-- "11:59" 719
-- "13:30" 810
-- "23:32" 1412
--
-- "12:00" 720
--
split :: String -> [String]
split [] = [""]
split (x:xs)
  | x == ':'  = "" : rest
  | otherwise = (x : head rest) : tail rest
  where
    rest = split xs


minutes :: [String] -> Int
minutes []          = error "No head for empty lists!"
minutes [""]        = error "No empty string!"
minutes (x : y : _) = 60 * (read x)  + (read y) :: Int



diff :: Int -> Int -> Int
diff x y = y - x


min' :: [Int] -> Int
min' [] = error "No items!"
min' (x:xs)
    | x < 0     = min' xs
    | otherwise = minimum (x:xs)


someFunc :: IO ()
someFunc = do
    let currentTime = minutes $ split "12:00"
        subwayTimes = ["05:31","11:59","13:30","23:32"]
        mappedTimes = map (\x -> split x) subwayTimes
        timedTimes  = map (\x -> minutes x) mappedTimes
    putStrLn $ show $  min' $ map (diff currentTime) timedTimes

