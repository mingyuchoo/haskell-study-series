{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module Lib
    ( someFunc
    ) where

import           Data.Hashable (Hashable (hash))
import           Data.Kind     (Type)
import           Data.Word     (Word8)

import           GHC.Generics  (Generic)

-- |
--
type Color :: Type
data Color = Color { red   :: Word8
                   , green :: Word8
                   , blue  :: Word8
                   }
     deriving stock (Eq, Generic, Show)
     deriving anyclass (Hashable)

-- |
--
someFunc :: IO ()
someFunc =
  print (hash (Color 255 0 0)) >>
  print (hash (Color 0 255 0)) >>
  print (hash (Color 0 0 255))
