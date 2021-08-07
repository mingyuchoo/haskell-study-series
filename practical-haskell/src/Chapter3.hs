{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE UnicodeSyntax  #-}
--------------------------------------------------------------------------------
module Chapter3
    where

--------------------------------------------------------------------------------
import           Data.List hiding (head, tail)
import qualified Data.List as L (filter, permutations)

--------------------------------------------------------------------------------
-- Parametric Polymorphism
-- Higher-Order Functions
-- Exporting, Importing


--------------------------------------------------------------------------------
-- | maybeString
--
-- Examples:
--
-- >>> :t maybeString
-- maybeString :: Maybe a -> [Char]
--
-- >>> maybeString (Just "a")
-- "Just"
--
-- >>> maybeString Nothing
-- "Nothing"
--
maybeString (Just _) = "Just"
maybeString Nothing  = "Nothing"

--------------------------------------------------------------------------------
-- | Client
--
-- Examples:
--
-- >>> :t GovOrg 'n' "NTTF"
-- GovOrg 'n' "NTTF" :: Client Char
data Client i = GovOrg     { clientId :: i, clientName :: String}
              | Company    { clientId :: i, clientName :: String, person :: Person, duty :: String }
              | Individual { clientId :: i,                       person :: Person }
              deriving (Show, Eq, Ord)
data Person = Person { firstName :: String, lastName :: String}
            deriving (Show, Eq, Ord)

data Triple a b c = Triple a b c

data SamePair a = SamePair a a a a a a a

--------------------------------------------------------------------------------
-- | map and succ
--
-- Examples:
--
-- >>> succ 1
-- 2
--
-- >>> map succ [1,2,3]
-- [2,3,4]
-- >>> :t map
-- map :: (a -> b) -> [a] -> [b]

-- | apply3f2
--
-- Examples:
--
-- >>> apply3f2 succ 7
-- 30
--
apply3f2 :: (Integer -> Integer) -> Integer -> Integer
apply3f2 f x = 3 * f (x + 2)

--------------------------------------------------------------------------------
-- | block
--
-- Examples:
--
-- >>> let f x = x + 2; in map f [1,2,3]
-- [3,4,5]
-- >>> map (\x -> x + 2) [1,2,3]
-- [3,4,5]

-- | equalTuples
--
-- Examples:
--
-- >>> equalTuples [(0,0),(0,1),(1,1),(2,1)]
-- [True,False,True,False]
--
equalTuples :: [(Integer, Integer)] -> [Bool]
equalTuples t = map (\(x,y) -> x == y) t

-- | sayHello
--
-- Examples:
--
-- >>> sayHello ["Alejandro", "Choo"]
-- ["Hello, writer","Welcome, Choo"]
--
sayHello :: [String] -> [String]
sayHello names = map (\name -> case name of
                                 "Alejandro" -> "Hello, writer"
                                 _           -> "Welcome, " ++ name
                     ) names

-- | sayHello'
--
-- Examples:
--
-- >>> sayHello' ["Alejandro", "Choo"]
-- ["Hello, writer","Welcome, Choo"]
--
sayHello' :: [String] -> [String]
sayHello' names = map (\case "Alejandro" -> "Hello, writer"
                             name        -> "Welcome, " ++ name
                      ) names

-- | multiplyByN
--
-- Examples:
--
-- >>> multiplyByN 2 3
-- 6
-- >>> map (multiplyByN 5) [1,2,3]
-- [5,10,15]
--
multiplyByN :: Integer -> (Integer -> Integer)
-- multiplyByN n = \x -> n * x
multiplyByN n = (n *)

-- | filter & even
--
-- Examples:
--
-- >>> filter even [1,2,3,4,5]
-- [2,4]
--

--------------------------------------------------------------------------------
-- | double
--
-- Examples:
--
-- >>> double [1,2,3,4,5]
-- [2,4,6,8,10]
-- >>> double' [1,2,3,4,5]
-- [2,4,6,8,10]
-- >>> double'' [1,2,3,4,5]
-- [2,4,6,8,10]
-- >>> double''' [1,2,3,4,5]
-- [2,4,6,8,10]
---
double :: Num b => [b] -> [b]
double list  = map (\x -> x * 2) list

double' :: [Integer] -> [Integer]
double'      = \list -> map (\x -> x * 2) list

double'' :: [Integer] -> [Integer]
double''     = map (\x -> x * 2)

double''' :: [Integer] -> [Integer]
double'''    = map (* 2)

-- | map
--
-- Examples:
--
-- >>> map (/2) [1,2,3]
-- [0.5,1.0,1.5]
-- >>> map (2/) [1,2,3]
-- [2.0,1.0,0.6666666666666666]
-- >>> :t Just
-- Just :: a -> Maybe a
-- >>> :t ('a' :)
-- ('a' :) :: [Char] -> [Char]
--


-- | duplicateOdds
--
-- Examples:
--
-- >>> duplicateOdds [1,2,3,4,5,6,7,8,9]
-- [2,6,10,14,18]
--
duplicateOdds :: Integral b => [b] -> [b]
duplicateOdds list = map (*2) $ filter odd list

-- | duplicateOdds'
--
-- Examples:
--
-- >>> duplicateOdds' [1,2,3,4,5,6,7,8,9]
-- [2,6,10,14,18]
--
duplicateOdds' :: [Integer] -> [Integer]
duplicateOdds' = map (*2) . filter odd

-- | uncurry
--
-- Examples:
--
-- >>> (uncurry max) (3,2)
-- 3
-- >>> map (uncurry max) [(1,2),(2,1),(3,4)]
-- [2,2,4]
--

--------------------------------------------------------------------------------
-- More on Modules
--------------------------------------------------------------------------------
-- | permutationsStartingWith
--
-- Examples:
--
-- >>> permutationsStartingWith 'a' "abc"
-- ["abc","acb"]

permutationsStartingWith :: Char -> String -> [String]
permutationsStartingWith letter
  =  filter (\l -> head l == letter) . L.permutations

-- | range
--
-- Examples:
--
-- >>> range 1 100
-- Range 1 100
--
data Range = Range Integer Integer deriving Show

range :: Integer -> Integer -> Range
range a b = if a <= b
            then Range a b
            else error "a must be <= b"

-- | r
--
-- Examples:
--
-- >>> r (Range 1 10)
-- R 1 10
--
data RangeObs = R Integer Integer deriving Show

r :: Range -> RangeObs
r (Range a b) = R a b

