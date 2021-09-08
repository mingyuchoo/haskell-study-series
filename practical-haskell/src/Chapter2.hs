{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnicodeSyntax            #-}
{-# LANGUAGE ViewPatterns             #-}

--------------------------------------------------------------------------------
module Chapter2
  where

--------------------------------------------------------------------------------
import           Data.Char
--------------------------------------------------------------------------------
-- | firstOrEmpty
--
-- >>> firstOrEmpty []
-- "empty"
-- >>> firstOrEmpty ["hello","hola"]
-- "hello"
firstOrEmpty ∷ [String] -> String
firstOrEmpty list = if not (null list)
                      then head list
                      else "empty"

--------------------------------------------------------------------------------
-- | (+++)
(+++) ∷ [a] -> [a] -> [a]
list1 +++ list2 = if null list1 {- check emptyness -}
                    then list2    {- base case       -}
                    else head list1 : (tail list1 +++ list2)


-- | (+++!)
(+++!) ∷ [a] -> [a] -> [a]
list1 +++! list2 = case list1 of
                     []   -> list2
                     x:xs -> x:(xs +++! list2)


-- | (+++!!)
(+++!!) ∷ [a] -> [a] -> [a]
[]     +++!! list2 = list2
(x:xs) +++!! list2 = x:(xs +++!! list2)
--------------------------------------------------------------------------------
-- | reverse2
reverse2 ∷ [a] -> [a]
reverse2 list = if null list
                  then []
                  else reverse2 (tail list) +++ [head list]

--------------------------------------------------------------------------------
-- | maxmin
maxmin ∷ (Ord a) => [a] -> (a, a)
maxmin list = if null (tail list)
                then (head list, head list)
                else ( if head list > fst (maxmin (tail list))
                         then head list
                         else fst (maxmin (tail list))
                     , if head list < snd (maxmin (tail list))
                         then head list
                         else snd (maxmin (tail list))
                     )


-- | maxmin'
maxmin' ∷ (Ord a) => [a] -> (a, a)
maxmin' list = let h = head list
               in if null (tail list)
                    then (h, h)
                    else ( if h > t_max then h else t_max
                         , if h < t_min then h else t_min
                         )
                         where
                           t     = maxmin' (tail list)
                           t_max = fst t
                           t_min = snd t


-- | maxmin''
maxmin'' ∷ (Ord a) => [a] -> (a, a)
maxmin'' [x]    = (x, x)
maxmin'' (x:xs) = ( if x > xs_max then x else xs_max
                  , if x < xs_min then x else xs_min
                  )
                  where
                    (xs_max, xs_min) = maxmin'' xs

--------------------------------------------------------------------------------
-- https://wiki.haskell.org/Constructor
--
type Client :: *
data Client = GovOrg     String
            | Company    String Integer Person String
            | Individual Person Bool
            deriving (Show)


type Person :: *
data Person = Person String String Gender
            deriving (Show)

type Gender :: *
data Gender = Male
            | Femail
            | Unkown
            deriving (Show)

--------------------------------------------------------------------------------
-- | clientName
clientName ∷ Client -> String
clientName client = case client of
                      GovOrg name                 -> name
                      Company name id person resp -> name
                      Individual person ads       -> case person of
                                                       Person fistName lastName gender -> fistName ++ " " ++ lastName


-- | clientName2
clientName2 ∷ Client -> String
clientName2 (GovOrg name)                                = name
clientName2 (Company name _ _ _)                         = name
clientName2 (Individual (Person firstName lastName _) _) = firstName ++ " " ++ lastName


-- | clientName'
clientName' ∷ Client -> String
clientName' client = case client of
                       GovOrg name                               -> name
                       Company name _ _ _                        -> name
                       Individual (Person fistName lastName _) _ -> fistName ++ " " ++ lastName


-- | clientName''
clientName'' ∷ Client -> Maybe String
clientName'' client = case client of
                        Company name _ _ _ -> Just name
                        _                  -> Nothing

--------------------------------------------------------------------------------
-- | fibonacci
fibonacci ∷ Integer -> Integer
fibonacci n = case n of
                0 -> 0
                1 -> 1
                _ -> fibonacci (n - 1) + fibonacci (n - 2)


-- | fibonacci'
fibonacci' ∷ Integer -> Integer
fibonacci' 0 = 0
fibonacci' 1 = 1
fibonacci' n = fibonacci (n - 1) + fibonacci (n - 2)


-- | ifibonacci
ifibonacci ∷ Integer -> Maybe Integer
ifibonacci n = if n < 0
                 then Nothing
                 else case n of
                        0 -> Just 0
                        1 -> Just 1
                        n' -> let Just f1 = ifibonacci (n' - 1)
                                  Just f2 = ifibonacci (n' - 2)
                              in Just (f1 + f2)

-- | ifibonacci'
ifibonacci' ∷ Integer -> Maybe Integer
ifibonacci' n | n < 0     = Nothing
ifibonacci' 0             = Just 0
ifibonacci' 1             = Just 1
ifibonacci' n  = let Just f1 = ifibonacci' (n - 1)
                     Just f2 = ifibonacci' (n - 2)
                 in Just (f1 + f2)
--------------------------------------------------------------------------------
-- | f
f ∷ Client -> String
f client = case client of
           Company _ _ (Person name _ _) "Boss" -> name ++ " is the boss"
           _                                    -> "There is no boss"

-- | g
g ∷ Client -> String
g client = case client of
           Company _ _ (Person name _ _) pos -> case pos of
                                                  "Boss" -> name ++ " is the boss"
           _                                 -> "There is no boss"

--------------------------------------------------------------------------------
-- | sorted
sorted ∷ [Integer] -> Bool
sorted []       = True
sorted [_]      = True
sorted (x:y:zs) = x < y && sorted (y:zs)


-- | sorted'
sorted' ∷ [Integer] -> Bool
sorted' []          = True
sorted' [_]         = True
sorted' (x:r@(y:_)) = x < y && sorted' r


-- | sorted''
sorted'' ∷ [Integer] -> Bool
sorted'' list = case list of
                  []          -> True
                  [_]         -> True
                  (x:r@(y:_)) -> x < y && sorted'' r

--------------------------------------------------------------------------------
-- | binom
binom :: (Eq a, Num a, Num p) => a -> a -> p
binom _ 0          = 1
binom x y | x == y = 1
binom n k          = binom (n - 1) (k - 1) + binom (n - 1) k


--------------------------------------------------------------------------------
-- | multipleOf
--
-- >>> multipleOf 0 0
-- *** Exception: divide by zero
-- >>> multipleOf 1 0
-- *** Exception: divide by zero
-- >>> multipleOf 1 1
-- True
-- >>> multipleOf 1 2
-- False
-- >>> multipleOf 1 3
-- False
multipleOf ∷ Integer -> Integer -> Bool
multipleOf x y = mod x y == 0

-- | specialMultiples
--
-- >>> specialMultiples 2
-- "2 is multiple of 2"
-- >>> specialMultiples 3
-- "3 is multiple of 3"
-- >>> specialMultiples 4
-- "4 is multiple of 2"
-- >>> specialMultiples 5
-- "5 is multiple of 5"
-- >>> specialMultiples 7
-- "7 is beautiful number"
specialMultiples :: Integer -> String
specialMultiples n | multipleOf n 2 = show n ++ " is multiple of 2"
specialMultiples n | multipleOf n 3 = show n ++ " is multiple of 3"
specialMultiples n | multipleOf n 5 = show n ++ " is multiple of 5"
specialMultiples n       = show n ++ " is beautiful number"

-- | specialMultiples'
--
-- >>> specialMultiples' 2
-- "2 is multiple of 2"
-- >>> specialMultiples' 3
-- "3 is multiple of 3"
-- >>> specialMultiples' 4
-- "4 is multiple of 2"
-- >>> specialMultiples' 5
-- "5 is multiple of 5"
-- >>> specialMultiples' 7
-- "7 is beautiful number"
specialMultiples' n
  | multipleOf n 2 = show n ++ " is multiple of 2"
  | multipleOf n 3 = show n ++ " is multiple of 3"
  | multipleOf n 5 = show n ++ " is multiple of 5"
  | otherwise      = show n ++ " is beautiful number"

--------------------------------------------------------------------------------
-- | responsibility
--
-- >>> responsibility (Company "A" 5 (Person "John" "Do" Male) "Director")
-- "Director"
-- >>> responsibility (GovOrg "NASA")
-- "Unknown"
responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _                 = "Unknown"

-- | specialClient
--
-- >>> specialClient (Individual (Person "Mr." "Alejandro" Male) True)
-- True
specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director")  = True
specialClient _                               = False

--------------------------------------------------------------------------------
-- https://wiki.haskell.org/Constructor
--

-- | ClientR
--
-- >>> GovOrgR "NATO"
-- GovOrgR {clientRName = "NATO"}
-- >>> CompanyR { clientRName = "GE", companyId = 1, person = PersonR { firstName = "John", lastName = "Smith"}, duty = "CEO"}
-- CompanyR {clientRName = "GE", companyId = 1, person = PersonR {firstName = "John", lastName = "Smith"}, duty = "CEO"}
-- >>> IndividualR { person = PersonR { lastName = "Smith", firstName = "John"}}
-- IndividualR {person = PersonR {firstName = "John", lastName = "Smith"}}
-- >>> clientRName (GovOrgR "NATO")
-- "NATO"
type ClientR :: *
data ClientR = GovOrgR     { clientRName :: String }
             | CompanyR    { clientRName :: String
                           , companyId   :: Integer
                           , person      :: PersonR
                           , duty        :: String }
             | IndividualR { person :: PersonR }
             deriving Show

type PersonR :: *
data PersonR = PersonR { firstName :: String
                       , lastName  :: String }
             deriving Show

--------------------------------------------------------------------------------
-- | greet
--
-- >>> greet (GovOrgR "NATO")
-- "Welcome"
-- >>> greet (CompanyR {clientRName = "GE", companyId = 1, person = PersonR {firstName = "John", lastName = "Smith"}, duty = "CEO"})
-- "Hi, GE"
-- >>> greet (IndividualR { person = PersonR { lastName = "Smith", firstName = "John"}})
-- "Hi, John"
greet :: ClientR -> String
greet IndividualR { person = PersonR { firstName = fn }} = "Hi, " ++ fn
greet CompanyR    { clientRName = c }                    = "Hi, " ++ c
greet GovOrgR     { }                                    = "Welcome"

-- | greet'
--
-- >>> greet' (GovOrgR "NATO")
-- "Welcome"
-- >>> greet' (CompanyR {clientRName = "GE", companyId = 1, person = PersonR {firstName = "John", lastName = "Smith"}, duty = "CEO"})
-- "Hi, GE"
-- >>> greet' (IndividualR { person = PersonR { lastName = "Smith", firstName = "John"}})
-- "Hi, John"
greet' :: ClientR -> String
greet' IndividualR { person = PersonR { firstName }} = "Hi, " ++ firstName
greet' CompanyR    { clientRName }                   = "Hi, " ++ clientRName
greet' GovOrgR     { }                               = "Welcome"

-- | greet''
--
-- >>> greet'' (GovOrgR "NATO")
-- "Welcome"
-- >>> greet'' (CompanyR {clientRName = "GE", companyId = 1, person = PersonR {firstName = "John", lastName = "Smith"}, duty = "CEO"})
-- "Hi, GE"
-- >>> greet'' (IndividualR { person = PersonR { lastName = "Smith", firstName = "John"}})
-- "Hi, John"
greet'' :: ClientR -> String
greet'' IndividualR { person = PersonR { .. }} = "Hi, " ++ firstName
greet'' CompanyR    { .. }                     = "Hi, " ++ clientRName
greet'' GovOrgR     { }                        = "Welcome"

-- | nameInCapitals
--
-- >>> nameInCapitals (PersonR { lastName = "smith", firstName = "john"})
-- PersonR {firstName = "John", lastName = "smith"}
nameInCapitals :: PersonR -> PersonR
nameInCapitals p@PersonR { firstName = initial : rest } = let newName = toUpper initial : rest
                                                          in p { firstName = newName }
nameInCapitals p@PersonR { firstName = "" }             = p

--------------------------------------------------------------------------------
-- https://wiki.haskell.org/Constructor
--

type ConnType :: *
data ConnType = TCP
              | UDP

type UseProxy :: *
data UseProxy = NoProxy
              | Proxy String

type TimeOut :: *
data TimeOut = NoTimeOut
             | TimeOut Integer

type ConnOptions :: *
data ConnOptions = ConnOptions { connType      :: ConnType
                               , connSpeed     :: Integer
                               , connProxy     :: UseProxy
                               , connCaching   :: Bool
                               , connKeepAlive :: Bool
                               , connTimeOut   :: TimeOut
                               }
