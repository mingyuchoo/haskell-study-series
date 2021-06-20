module Chapter07.Modules
  (
  ) where

import Data.List

-- | numUniques
--
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
