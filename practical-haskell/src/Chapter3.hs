{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnicodeSyntax            #-}
{-# LANGUAGE ViewPatterns             #-}
--------------------------------------------------------------------------------
module Chapter3
    where

--------------------------------------------------------------------------------
import           Data.Function
import           Data.List     hiding (head, tail)
import qualified Data.List     as L (filter, partition, permutations)

--------------------------------------------------------------------------------
-- Parametric Polymorphism
-- Higher-Order Functions
-- Exporting, Importing

--------------------------------------------------------------------------------
-- | maybeString
--
-- >>> :t maybeString
-- maybeString :: Maybe a -> [Char]
-- >>> maybeString (Just "a")
-- "Just"
-- >>> maybeString Nothing
-- "Nothing"
maybeString (Just _) = "Just"
maybeString Nothing  = "Nothing"

--------------------------------------------------------------------------------
-- | Client
--
-- >>> :t GovOrg 'n' "NTTF"
-- GovOrg 'n' "NTTF" :: Client Char
type Client :: * -> *
data Client i = GovOrg     { clientId :: i, clientName :: String}
              | Company    { clientId :: i, clientName :: String, person :: Person, duty :: String }
              | Individual { clientId :: i,                       person :: Person }
              deriving (Show, Eq, Ord)

type Person :: *
data Person = Person { firstName :: String, lastName :: String}
            deriving (Show, Eq, Ord)

type Triple :: * -> * -> * -> *
data Triple a b c = Triple a b c
                  deriving Show

type SamePair :: * -> *
data SamePair a = SamePair a a a
                deriving Show

--------------------------------------------------------------------------------
-- | map and succ
--
-- >>> succ 1
-- 2
-- >>> map succ [1,2,3]
-- [2,3,4]
-- >>> :t map
-- map :: (a -> b) -> [a] -> [b]

-- | apply3f2
--
-- >>> apply3f2 succ 7
-- 30
apply3f2 :: (Integer -> Integer) -> Integer -> Integer
apply3f2 f x = 3 * f (x + 2)

--------------------------------------------------------------------------------
-- | block
--
-- >>> let f x = x + 2; in map f [1,2,3]
-- [3,4,5]
-- >>> map (\x -> x + 2) [1,2,3]
-- [3,4,5]

-- | equalTuples
--
-- >>> equalTuples [(0,0),(0,1),(1,1),(2,1)]
-- [True,False,True,False]
equalTuples :: [(Integer, Integer)] -> [Bool]
equalTuples t = map (\(x,y) -> x == y) t

-- | sayHello
--
-- >>> sayHello ["Alejandro", "Choo"]
-- ["Hello, writer","Welcome, Choo"]
sayHello :: [String] -> [String]
sayHello names = map (\name -> case name of
                                 "Alejandro" -> "Hello, writer"
                                 _           -> "Welcome, " ++ name
                     ) names

-- | sayHello'
--
-- >>> sayHello' ["Alejandro", "Choo"]
-- ["Hello, writer","Welcome, Choo"]
sayHello' :: [String] -> [String]
sayHello' names = map (\case "Alejandro" -> "Hello, writer"
                             name        -> "Welcome, " ++ name
                      ) names

-- | multiplyByN
--
-- >>> multiplyByN 2 3
-- 6
-- >>> map (multiplyByN 5) [1,2,3]
-- [5,10,15]
multiplyByN :: Integer -> (Integer -> Integer)
-- multiplyByN n = \x -> n * x
multiplyByN n = (n *)

-- | filter & even
--
-- >>> filter even [1,2,3,4,5]
-- [2,4]

--------------------------------------------------------------------------------
-- | double
--
-- >>> double [1,2,3,4,5]
-- [2,4,6,8,10]
-- >>> double' [1,2,3,4,5]
-- [2,4,6,8,10]
-- >>> double'' [1,2,3,4,5]
-- [2,4,6,8,10]
-- >>> double''' [1,2,3,4,5]
-- [2,4,6,8,10]
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
-- >>> map (/2) [1,2,3]
-- [0.5,1.0,1.5]
-- >>> map (2/) [1,2,3]
-- [2.0,1.0,0.6666666666666666]
-- >>> :t Just
-- Just :: a -> Maybe a
-- >>> :t ('a' :)
-- ('a' :) :: [Char] -> [Char]


-- | duplicateOdds
--
-- >>> duplicateOdds [1,2,3,4,5,6,7,8,9]
-- [2,6,10,14,18]
duplicateOdds :: Integral b => [b] -> [b]
duplicateOdds list = map (*2) $ filter odd list

-- | duplicateOdds'
--
-- >>> duplicateOdds' [1,2,3,4,5,6,7,8,9]
-- [2,6,10,14,18]
duplicateOdds' :: [Integer] -> [Integer]
duplicateOdds' = map (*2) . filter odd

-- | uncurry
--
-- >>> (uncurry max) (3,2)
-- 3
-- >>> map (uncurry max) [(1,2),(2,1),(3,4)]
-- [2,2,4]

--------------------------------------------------------------------------------
-- More on Modules
--------------------------------------------------------------------------------
-- | permutationsStartingWith
--
-- >>> permutationsStartingWith 'a' "abc"
-- ["abc","acb"]
permutationsStartingWith :: Char -> String -> [String]
permutationsStartingWith letter
  =  filter (\l -> head l == letter) . L.permutations

-- | Range
type Range :: *
data Range = Range Integer Integer deriving Show

-- | range
--
-- >>> range 1 100
-- Range 1 100
range :: Integer -> Integer -> Range
range a b = if a <= b
            then Range a b
            else error "a must be <= b"

-- | RangeObs
type RangeObs :: *
data RangeObs = R Integer Integer deriving Show

-- | r
--
-- >>> r (Range 1 10)
-- R 1 10
r :: Range -> RangeObs
r (Range a b) = R a b

-- | prettyRange
--
-- >>> prettyRange (Range 1 10)
-- "[1,10]"
-- >>> prettyRange (Range 10 1)
-- "[10,1]"
prettyRange :: Range -> String
prettyRange rng = case rng of
                    (r -> R a b) -> "[" ++ show a ++ "," ++ show b ++ "]"

-- | pattern R'
--
-- >>> R' 1 10
-- Range 1 10
pattern R' :: Integer -> Integer -> Range
pattern R' a b <- Range a b
  where R' a b = range a b

--------------------------------------------------------------------------------
-- Diving into Lists
--------------------------------------------------------------------------------

-- | foldr
--
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f initial [] = initial
-- foldr f initial (x:xs) = f x (foldr f initial xs)
-- >>> foldr (+) 0 [1,2,3]
-- 6

-- | infMax
--
-- >>> foldr infMax MinusInfinity $ map Number [1,2,3]
-- Number 3
-- >>> foldr (\x y -> infMax (Number x) y) MinusInfinity [1,2,3]
-- Number 3

type InfNumber :: * -> *
data InfNumber a = MinusInfinity
                 | Number a
                 | PlusInfinity
                 deriving Show

infMax MinusInfinity x       = x
infMax x MinusInfinity       = x
infMax PlusInfinity _        = PlusInfinity
infMax _ PlusInfinity        = PlusInfinity
infMax (Number a) (Number b) = Number (max a b)

-- | foldl
--
-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- foldl _ initial []     = initial
-- foldl f initial (x:xs) = foldl f (f initial x) xs
--
-- >>> foldl (+) 0 [1,2,3]
-- 6
-- >>> foldr (-) 0 [1,2,3]
-- 2
-- >>> foldl (-) 0 [1,2,3]
-- -6



--------------------------------------------------------------------------------
-- | maximum'
--
-- >>> maximum' [1,2,3,4,5]
-- 5
maximum' :: [Integer] -> Integer
maximum' = foldr1 max

-- | bothFilters
--
-- >>> bothFilters (> 5) [1,2,3,4,5,6,7,8,9,10]
-- ([6,7,8,9,10],[1,2,3,4,5])
bothFilters :: (a -> Bool) -> [a] -> ([a],[a])
bothFilters p list = (filter p list, filter (not . p) list)


--------------------------------------------------------------------------------
-- | import Data.List
-- | partition :: (a -> Bool) -> [a] -> [a] -> ([a], [a])
--
-- >>> partition  (> 5) [1,2,3,4,5,6,7,8,9,10]
-- ([6,7,8,9,10],[1,2,3,4,5])


-- | find :: Foldable t => (a -> Bool) -> t a -> Maybe a
--
-- >>> find (> 5) [1,2,3,4,5,6,7,8,9,10]
-- Just 6
-- >>> find (> 0) [1,2,-3,4,-5,6]
-- Just 1
-- >>> find (> 7)  [1,2,-3,4,-5,6]
-- Nothing


--------------------------------------------------------------------------------
-- | import Data.List
-- | dropWhile :: (a -> Bool) -> [a] -> [a]
--
-- >>> dropWhile (> 5) [1,2,3,4,5,6,7,8,9,10]
-- [1,2,3,4,5,6,7,8,9,10]
-- >>> dropWhile (< 5) [1,2,3,4,5,6,7,8,9,10]
-- [5,6,7,8,9,10]
-- >>> dropWhile (> 5) [10,9,8,7,6,5,1,2,3,4,5,6,7,8,9,10]
-- [5,1,2,3,4,5,6,7,8,9,10]


-- | skipUntilGov
skipUntilGov :: [Client a] -> [Client a]
skipUntilGov = dropWhile (\case { GovOrg {} -> False; _ -> True })

-- | import Data.List
-- | takeWhile :: (a -> Bool) -> [a] -> [a]
--
-- >>> takeWhile (/= "stop") ["hello", "send", "stop", "receive"]
-- ["hello","send"]


--------------------------------------------------------------------------------
-- | import Data.List
-- | span :: (a -> Bool) -> [a] -> ([a], [a])
--
-- >>> span (< 5) [1,2,3,4,5,6,7,8,9,10]
-- ([1,2,3,4],[5,6,7,8,9,10])
-- >>> span (> 5) [1,2,3,4,5,6,7,8,9,10]
-- ([],[1,2,3,4,5,6,7,8,9,10])
-- >>> span (== 5) [1,2,3,4,5,6,7,8,9,10]
-- ([],[1,2,3,4,5,6,7,8,9,10])
-- >>> span (> 5) [10,9,8,7,6,5,4,3,2,1]
-- ([10,9,8,7,6],[5,4,3,2,1])
-- >>> span (/= "stop") ["hello", "send", "stop", "receive"]
-- (["hello","send"],["stop","receive"])


--------------------------------------------------------------------------------
-- | isIndividual
isIndividual :: Client a -> Bool
isIndividual (Individual {}) = True
isIndividual _               = False


-- | checkAnalytics
-- | any :: Foldable t => (a -> Bool) -> t a -> Bool
-- | all :: Foldable t => (a -> Bool) -> t a -> Bool
checkAnalytics :: [Client a] -> (Bool, Bool)
checkAnalytics cs = (any isIndividual cs, not $ all isIndividual cs)

--------------------------------------------------------------------------------
-- >>> nub [1,2,1,1,3,2,4,1]
--
-- | nubBy :: (a -> a -> Bool) -> [a] -> [a]
--
-- >>> let p x y = (even x && even y) || (odd x && odd y)
-- >>> nubBy p [1,2,3,4,5]
-- [1,2]
-- >>> nubBy (==) [1,2,1,1,3,2,4,1]
-- [1,2,3,4]


-- | nub :: Eq a => [a] -> [a]
--
-- >>> nub [1,2,1,1,3,2,4,1]
-- [1,2,3,4]


--------------------------------------------------------------------------------
-- | union :: Eq a => [a] -> [a] -> [a]
-- | A ∪ B
--
-- >>> [1,2,3,4] `union` [2,3,5]
-- [1,2,3,4,5]


-- | intersect :: Eq a => [a] -> [a] -> [a]
-- | A ∩ B
--
-- >>> [1,2,3,4] `intersect` [2,3,5]
-- [2,3]


-- | (\\) :: Eq a => [a] -> [a] -> [a]
-- | A - B
--
-- >>> [1,2,3,4] \\ [2,3,5]
-- [1,4]


-- | elem :: (Foldable t, Eq a) => a -> t a -> Bool
--
-- >>> 2 `elem` [1,2,3]
-- True
-- >>> 4 `elem` [1,2,3]
-- False


--------------------------------------------------------------------------------

-- | comapreClient
--
-- >>> sortBy comapreClient listOfClients
-- [GovOrg {clientId = 3, clientName = "NTTF"},Company {clientId = 4, clientName = "Wormhole Inc.", person = Person {firstName = "Karl", lastName = "Schwarzschild"}, duty = "Physicist"},Individual {clientId = 6, person = Person {firstName = "Sarah", lastName = "Jane"}},Individual {clientId = 5, person = Person {firstName = "Doctor", lastName = ""}},Individual {clientId = 2, person = Person {firstName = "H. G.", lastName = "Wells"}}]
--
compareClient :: Client a -> Client a -> Ordering
compareClient (Individual {person = p1}) (Individual {person = p2}) = compare (firstName p1) (firstName p2)
comapreClient (Individual {})            _                          = GT
comapreClient _                          (Individual {})            = LT
comapreClient c1                         c2                         = compare (clientName c1) (clientName c2)


listOfClients
  = [ Individual 2 (Person "H. G." "Wells")
    , GovOrg 3 "NTTF" -- National Time Travel Foundation
    , Company 4 "Wormhole Inc." (Person "Karl" "Schwarzschild") "Physicist"
    , Individual 5 (Person "Doctor" "")
    , Individual 6 (Person "Sarah" "Jane")
    ]

--------------------------------------------------------------------------------
companyDutiesAnalytics :: [Client a] -> [String]
companyDutiesAnalytics = map (duty . head) .
                           sortBy (\x y -> compare (length y) (length x)) .
                           groupBy (\x y -> duty x == duty y) .
                           filter isCompany
                       where isCompany (Company {}) = True
                             isCompany _            = False


companyDutiesAnalytics' :: [Client a] -> [String]
companyDutiesAnalytics' = map (duty . head) .
                            sortBy (flip (compare `on` length)) .
                            groupBy ((==) `on` duty) .
                            filter isCompany
                        where isCompany (Company {}) = True
                              isCompany _            = False

--------------------------------------------------------------------------------
-- | enum
--
-- >>> enum 0 (-1)
-- []
-- >>> enum 0 0
-- [0]
-- >>> enum 1 (0)
-- []
-- >>> enum 1 1
-- [1]
-- >>> enum 1 2
-- [1,2]
enum :: Int -> Int -> [Int]
enum a b | a > b = []
enum a b         = a : enum (a + 1) b

-- | withPositions
--
-- >>> withPositions ['a']
-- [(1,'a')]
-- >>> withPositions ['a','b']
-- [(1,'a'),(2,'b')]
withPositions :: [a] -> [(Int,a)]
withPositions list = zip (enum 1 $ length list) list

-- | withPositions'
--
-- >>> withPositions ['a']
-- [(1,'a')]
withPositions' :: [a] -> [(Int,a)]
withPositions' list = zip [1 .. length list] list

-- | unzip
--
-- >>> unzip [("France","Paris"),("Spain","Madrid"),("Portugal","Lisbon")]
-- (["France","Spain","Portugal"],["Paris","Madrid","Lisbon"])

-- | lookup
--
-- >>> lookup "Spain" [("France","Paris"),("Spain","Madrid"),("Portugal","Lisbon")]
-- Just "Madrid"
-- >>> lookup "UK" [("France","Paris"),("Spain","Madrid"),("Portugal","Lisbon")]
-- Nothing

