module Lib
    ( someFunc
    ) where

import           Data.Kind (Type)

-- | Higher-Order Functions
-- -----------------------------------------------

increment :: [Int] -> [Int]
increment = map (+ 1)

incrementBy :: Int -> [Int] -> [Int]
incrementBy x = map (+ x)

double :: [Int] -> [Int]
double = map (* 2)

square :: [Int] -> [Int]
square = map (^ 2)

-- | Defunctionalization from HOF
-- -----------------------------------------------

-- Step 1: Find all functions
-- >> increment, incrementBy, double, square, ...

-- Step 2: Make SumDataType representing the all functions
type ListOperation :: Type
data ListOperation = Increment
                   | IncrementBy Int
                   | Double
                   | Square

-- Step 3: Change function calls to "appy SumDataType"
applyOperation :: ListOperation -> [Int] -> [Int]
applyOperation Increment       = increment
applyOperation (IncrementBy x) = incrementBy x
applyOperation Double          = double
applyOperation Square          = square

-- | Main
-- -----------------------------------------------
someFunc :: IO ()

someFunc = do
  let nums :: [Int]
      nums = [1, 2, 3, 4]

  -- Higher-Order Functions
  print $ increment nums -- [2,3,4,5]
  print $ incrementBy 3 nums -- [4,5,6,7]
  print $ double nums -- [2,4,6,8]
  print $ square nums -- [1,4,9,16]
  print "-------------------"

  -- Defunctionalization the HOF
  print $ applyOperation Increment nums -- [2,3,4,5]
  print $ applyOperation (IncrementBy 3) nums -- [4,5,6,7]
  print $ applyOperation Double nums -- [2,4,6,8]
  print $ applyOperation Square nums -- [1,4,9,16]
