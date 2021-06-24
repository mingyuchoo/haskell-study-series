module Lib
    ( someFunc
    , TrafficLight(..)
    , Week(..)
    , Shape(..)
    ) where

class Same a where
    same :: a -> a -> Bool

data TrafficLight = Red | Amber | Green deriving (Eq, Show)
data Week = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
data Shape = Circle Float Float Float | Rectangle Float Float Float Float

instance Same TrafficLight where
    same Red Red     = Red   == Red
    same Amber Amber = Amber == Amber
    same Green Green = Green == Green
    same _ _         = False

someFunc :: IO ()
someFunc = print "Hello, World"
