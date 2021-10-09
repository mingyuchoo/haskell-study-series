module Chapter08.MakingOurOwnTypesAndTypeclasses
    where
--------------------------------------------------------------------------------

import qualified Data.Map as Map
--------------------------------------------------------------------------------

{--
 -- Algebric data types intro
--}

--------------------------------------------------------------------------------
data Point = Point Float Float
     deriving (Show)

--------------------------------------------------------------------------------
data Shape = Circle Point Float
           | Rectangle Point Point
     deriving (Show)

--------------------------------------------------------------------------------
-- | area
area :: Shape -> Float -- function signature declaration
area (Circle _ r)                            = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

--------------------------------------------------------------------------------
-- | nudge
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
  Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

--------------------------------------------------------------------------------
-- | baseCircle
baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

--------------------------------------------------------------------------------
-- | baseRect
baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

--------------------------------------------------------------------------------
{--
 -- Record syntax
--}

--------------------------------------------------------------------------------
data Person = Person String String Int Float String String
     deriving (Show)

--------------------------------------------------------------------------------
-- firstName :: Person -> String
-- firstName (Person firstname _ _ _ _ _) = firstname
--
-- lastName :: Person -> String
-- lastName (Person _ lastname _ _ _ _) = lastname
--
-- age :: Person -> Int
-- age (Person _ _ age _ _ _) = age
--
-- height :: Person -> Float
-- height (Person _ _ _ height _ _) = height
--
-- phoneNumber :: Person -> String
-- phoneNumber (Person _ _ _ _ number _) = number
--
-- flavor :: Person -> String
-- flavor (Person _ _ _ _ _ flavor) = flavor


--------------------------------------------------------------------------------
data Person2 = Person2 { firstName2   :: String
                       , lastName2    :: String
                       , age2         :: Int
                       , height2      :: Float
                       , phoneNumber2 :: String
                       , flavor2      :: String
                       }
     deriving (Show)

--------------------------------------------------------------------------------

data Car = Car { company :: String
               , model   :: String
               , year    :: Int
               }
     deriving (Show)

--------------------------------------------------------------------------------
-- tellCar :: Car -> String
-- tellCar (Car { company = c, model = m, year = y}) =
--   "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
--------------------------------------------------------------------------------
data Car2 a b c = Car2 { company2 :: a
                       , model2   :: b
                       , year2    :: c
                       }
     deriving (Show)

tellCar2 :: (Show a) => Car2 String String a -> String
tellCar2 (Car2 {company2 = c, model2 = m, year2 = y}) =
  "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

--------------------------------------------------------------------------------

data Vector a = Vector a a a
     deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i * m) (j * m) (k * m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i * l + j * m + k * m

--------------------------------------------------------------------------------


data Person3 = Person3 { firstName3 :: String
                       , lastName3  :: String
                       , age3       :: Int
                       }
     deriving (Eq)

--------------------------------------------------------------------------------

data Person4 = Person4 { firstName4 :: String
                       , lastName4  :: String
                       , age4       :: Int
                       }
     deriving (Eq, Read, Show)

--------------------------------------------------------------------------------

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving
     ( Bounded
     , Enum
     , Eq
     , Ord
     , Read
     , Show
     )

--------------------------------------------------------------------------------


type Name = String

type PhoneNumber = String

type PhoneBook = [(Name, PhoneNumber)]

--------------------------------------------------------------------------------

phoneBook :: PhoneBook
phoneBook = [ ("betty", "555-2938")
            , ("bonnie", "452-2928")
            , ("patsy", "493-2928")
            , ("lucille", "205-2928")
            , ("wendy", "939-8282")
            , ("penny", "853-2492") ]

--------------------------------------------------------------------------------

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

--------------------------------------------------------------------------------

type AssocList k v = [(k, v)]

--------------------------------------------------------------------------------

data LockerState = Taken | Free deriving (Eq, Show)

--------------------------------------------------------------------------------

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

--------------------------------------------------------------------------------

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) ->
      if state /= Taken
        then Right code
        else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

--------------------------------------------------------------------------------

lockers :: LockerMap
lockers = Map.fromList [ (100, (Taken, "ZD39I"))
                       , (101, (Free, "JAH3I"))
                       , (102, (Free, "IQSA9"))
                       , (103, (Free, "QOTSA"))
                       , (104, (Taken, "893JJ"))
                       , (105, (Taken, "88292"))
                       ]

--------------------------------------------------------------------------------
-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- data List a = Empty | Cons { listHead :: a, listTail :: List a}
--   deriving (Show, Read, Eq, Ord)
-- fixity
infixr 5 :-:


data List a = Empty
            | a :-: (List a)
     deriving (Eq, Ord, Read, Show)

-- fixity
infixr 5 .++


(.++) :: List a -> List a -> List a
Empty .++ ys      = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)


data Tree a = EmptyTree
            | Node a (Tree a) (Tree a)
     deriving (Eq, Read, Show)


singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree


treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right) | x == a = Node x left right
                                 | x < a  = Node a (treeInsert x left) right
                                 | x > a  = Node a left (treeInsert x right)


treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right) | x == a = True
                               | x < a  = treeElem x left
                               | x > a  = treeElem x right

--------------------------------------------------------------------------------

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red       = True
  Green == Green   = True
  Yellow == Yellow = True
  _ == _           = False

instance Show TrafficLight where
  show Red    = "Red light"
  show Yellow = "Yellow light"
  show Green  = "Green light"

--------------------------------------------------------------------------------
-- class declaration
--  - YesNo: typeclass
--  - yesno: function
class YesNo a where
  yesno :: a -> Bool


instance YesNo Int where
  yesno 0 = False
  yesno _ = True


instance YesNo [a] where
  yesno [] = False
  yesno _  = True


instance YesNo Bool where
  yesno = id


instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing  = False


yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal
                                        then yesResult
                                        else noResult


instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x leftsub rightsub) =
    Node (f x) (fmap f leftsub) (fmap f rightsub)


class Tofu t where
  tofu :: j a -> t a j


data Frank a b = Frank { frankField :: b a
                       }
     deriving (Show)


instance Tofu Frank where
  tofu x = Frank x


data Barry t k p = Barry { yabba :: p
                         , dabba :: t k
                         }


instance Functor (Barry a b) where
  fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}

--------------------------------------------------------------------------------
