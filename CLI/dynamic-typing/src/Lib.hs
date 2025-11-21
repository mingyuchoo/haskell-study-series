module Lib
    ( someFunc
    ) where

import           Data.Dynamic  (Dynamic, dynTypeRep, fromDynamic, toDyn)
import           Data.Foldable (asum, for_)
import           Data.Kind     (Type)

import           Flow          ((<|))

-- |
--
mixedList :: [Dynamic]
mixedList = [ toDyn True
            , toDyn (5 :: Integer)
            , toDyn "Hey"
            ]

-- |
--
someFunc :: IO ()
someFunc =  for_ mixedList <| \d -> putStrLn <| message d

-- |
--
message :: Dynamic -> String
message d = case recognizedType d of
  Just x  -> x
  Nothing -> "Unrecognized type: " <> (show <| dynTypeRep d)

-- |
--
recognizedType :: Dynamic -> Maybe String
recognizedType d = asum [ (fromDynamic d :: Maybe Integer) >>= \x -> Just (show x <> " is an integer")
                        , (fromDynamic d :: Maybe Bool) >>= \x -> Just ("The answer is " <> (if x then "yes" else "no"))
                        ]
