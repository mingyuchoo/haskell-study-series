{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}

module Lib
    ( someFunc
    ) where

import qualified Data.Map.Strict        as Map
import           Flow                   ((<|))
import           Generics.Deriving.Enum (GEnum (genum))
import           GHC.Generics           (Generic)

data Product = Basic
             | Standard
             | Pro
             deriving stock (Generic, Show)
             deriving anyclass GEnum

data Frequency = Monthly
               | Annual
               deriving stock (Generic, Show)
               deriving anyclass GEnum

data Bill = Bill Product Frequency
          deriving stock (Generic, Show)
          deriving anyclass GEnum

encodeProduct :: Product -> String
encodeProduct = \case
  Basic    -> "p1"
  Standard -> "p2"
  Pro      -> "p3"

encodeBill :: Bill -> Integer
encodeBill = \case
  Bill Basic Monthly    -> 10
  Bill Basic Annual     -> 11
  Bill Standard Monthly -> 20
  Bill Standard Annual  -> 21
  Bill Pro Monthly      -> 30
  Bill Pro Annual       -> 31

invert :: (GEnum a, Ord b) => (a -> b) -> b -> Maybe a
invert f =
    \b -> Map.lookup b reverseMap
  where
    reverseMap = foldMap (\a -> Map.singleton (f a) a) genum

decodeProduct :: String -> Maybe Product
decodeProduct = invert encodeProduct

decodeBill :: Integer -> Maybe Bill
decodeBill = invert encodeBill

someFunc :: IO ()
someFunc = do
  putStrLn <| encodeProduct Basic
  putStrLn <| encodeProduct Standard

  print <| decodeProduct "p1"
  print <| decodeProduct "xyz"

  print <| encodeBill (Bill Basic Annual)
  print <| encodeBill (Bill Pro Monthly)

  print <| decodeBill 31
  print <| decodeBill 50
