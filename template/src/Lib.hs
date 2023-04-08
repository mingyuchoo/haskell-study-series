
module Lib
    ( Checkable
    , Located (..)
    , Mammal (..)
    , Mammal2 (..)
    , Movable (..)
    , Move
    , MyTypeClass (..)
    , MyTypeConstructor (..)
    , NamedPoint (..)
    , Shape (..)
    , TrafficLight (..)
    , Week (..)
    , YesNo
    , checkIf
    , fly
    , move
    , myOtherFunction
    , same
    , someFunc
    , swim
    , walk
    , yesno
    , yesnoIf
    ) where

import           Data.Kind (Constraint, Type)

-- |
-- https://wiki.haskell.org/Constructor
--

-- |
-- :kind MyTypeConstructor :: *
-- :type MyDataConstructor :: MyTypeConstructor
type MyTypeConstructor :: *
data MyTypeConstructor = MyDataConstructor
                       deriving (Show)

-- |
-- :kind MyTypeClass :: * -> Constraint
-- :type myTypeClassFunction :: MyTypeClass myTypeVariable => myTypeVariable -> String
type MyTypeClass :: * -> Constraint
class MyTypeClass myTypeVariable where
  myTypeClassFunction :: myTypeVariable -> String

-- |
-- :kind MyTypeClass :: * -> Constraint
-- :type myTypeClassFunction :: MyTypeClass myTypeVariable => myTypeVariable -> String
-- :kind MyTypeConstructor :: *
instance MyTypeClass MyTypeConstructor where
  myTypeClassFunction MyDataConstructor = "MyValue"

-- |
--
--
myOtherFunction :: (MyTypeClass myTypeVariable) => myTypeVariable -> String
myOtherFunction x = "MyValue"

-- |
-- Class Inheritance (A concerted example)
-- https://en.wikibooks.org/wiki/Haskell/Classes_and_types
-- https://wiki.haskell.org/Constructor

-- |
-- Location, in two dimesions.
-- :kind Located             :: * -> Constraint
-- :kind Located Int         :: Constraint
-- :kind Located (Maybe Int) :: Constraint
-- :type getLocation :: Located a => a -> (Int, Int)
type Located :: * -> Constraint
class Located a where
  getLocation :: a -> (Int, Int)

-- |
-- :kind Movable :: * -> Constraint
-- :type setLocation :: Movable a => (Int, Int) -> a -> a
type Movable :: * -> Constraint
class (Located a) => Movable a where
  setLocation :: (Int, Int) -> a -> a

-- |
-- An example type, with accompanying instances.
-- :kind NamedPoint :: *
--            ^-- This is Type Constructor
-- :type NamedPoint :: String -> Int -> Int -> NamedPoint
--            ^-- This is Data Constructor
type NamedPoint :: *
data NamedPoint = NamedPoint { pointName :: String
                             , pointX    :: Int
                             , pointY    :: Int }
                deriving (Show)


-- |
-- :kind NamedPoint :: *
-- :kind Located :: * -> Constraint
-- :type getLocation :: Located p => p -> (Int, Int)
-- :type getLocation (NamedPoint "a" 1 1) :: (Int, Int)
instance Located NamedPoint where
  getLocation p = (pointX p, pointY p)

-- |
-- :kind NamedPoint :: *
-- :kind Movable :: * -> Constraint
-- :kind Movable NamedPoint :: Constraint
-- :type setLocation :: Movable a => (Int, Int) a -> a
-- :type setLocation (1, 1) :: Movable a => a -> a
-- :type setLocation (1, 1) (NamedPoint "a" 1 1) :: NamedPoint
instance Movable NamedPoint where
  setLocation (x, y) p = p { pointX = x, pointY = y}

-- |
-- Moves a value of a Movable type by the specified displacement
-- This works for any movable, including NamedPoint.
-- >>> move (1, 1) (NamedPoint "a" 2 2)
-- NamedPoint {pointName = "a", pointX = 3, pointY = 3}
move :: (Movable a) => (Int, Int) -> a -> a
move (dx, dy) p = setLocation (x + dx, y + dy) p where
                    (x, y) = getLocation p

-- |
-- 'TrafficLight' is a `type constructor` if has zero arguments just called a `type`
-- 'Red',...      are `data(value) constructors` if has zero arguments just called a `constant`
type TrafficLight :: *
data TrafficLight = Red
                  | Amber
                  | Green
                  deriving (Eq, Show)

-- |
-- 'Checkable' is a `type class` or just a `class`
-- 'a'         is a `type variable`
type Checkable :: * -> Constraint
class Checkable a where
    same :: a -> a -> Bool

-- |
-- 'Checkable'     is a `type class` or just a `class`
-- 'TrafficLight'  is a `type constructor` if has zero arguments just called a `type` used as a `type variable`
-- 'Red',...       are `data(value) constructors`
instance Checkable TrafficLight where
    same Red   Red   = True
    same Amber Amber = True
    same Green Green = True
    same _     _     = False

-- |
-- 'Checkable'     is a `type class constraint` in a function declaration
-- 'a'             is a `type variable`         in a function declaration
checkIf :: (Checkable a) => a -> a -> Bool
checkIf x y  = same x y                         -- 'x', 'y' are `bind variables`


-- |
--
--
type YesNo :: * -> Constraint
class YesNo a where
  yesno :: a -> Bool

-- |
-- >>> yesno (0 :: Int)
-- False
-- >>> yesno (3 :: Int)
-- True
instance YesNo Int where
  yesno x | x == 0 = False
          | otherwise = True

-- |
-- >>> yesno []
-- False
-- >>> yesno [1,2,3,4]
-- True
instance (Eq a) => YesNo [a] where
  yesno x | x == [] = False
          | otherwise = True

-- |
-- >>> yesno True
-- True
-- >>> yesno False
-- False
instance YesNo Bool where
  yesno = id

-- |
-- :kind YesNo :: * -> Constraint
-- :kind YesNo (Maybe Int) :: Constraint
-- >>> yesno Nothing
-- False
-- >>> yesno (Just 1)
-- True
instance YesNo (Maybe a) where
  yesno Nothing  = False
  yesno (Just _) = True

-- |
-- :kind TrafficLight :: *
-- :kind YesNo :: * -> Constraint
-- :kind YesNo TrafficLight :: Constraint
-- :type yesno :: YesNo a => a -> Bool
-- :type yesno Red :: Bool
-- >>> yesno Red
-- False
-- >>> yesno Green
-- True
instance YesNo TrafficLight where
  yesno x | x == Red  = False
          | otherwise = True


-- |
--
--
yesnoIf :: (YesNo a) => a -> b -> b -> b
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal
                                        then yesResult
                                        else noResult

-- >>>>> yesnoIf Red "GO" "STOP" >>> "STOP"


-- |
-- 'Mammal'  is a `type constructor` if has zero arguments just called a `type`
-- 'Bat',... are `data(value) constructors` if has zero arguments just called a `constant`
type Mammal :: *
data Mammal = Bat
            | Dolphin
            | Elephant
            | Human

type Mammal2 :: *
data Mammal2 = Bat2
            | Dolphin2
            | Elephant2
            | Human2
            deriving (Eq)

-- |
-- 'Move' is a `type class` or just a `class`
-- 'a'    is a `type variable`
type Move :: * -> Constraint
class Move a where
  swim :: a -> Bool
  walk :: a -> Bool
  fly  :: a -> Bool

-- |
-- 'Move'    is a `type class` or just a `class`
-- 'Mammal'  is a `type constructor` if has zero arguments just called a `type` used as a `type variable`
-- 'Bat',... are `data(value) constructors`
instance Move Mammal where
   swim Bat      = False
   swim Dolphin  = True
   swim Elephant = False
   swim Human    = True

   walk Bat      = False
   walk Dolphin  = False
   walk Elephant = True
   walk Human    = True

   fly  Bat      = True
   fly  Dolphin  = False
   fly  Elephant = False
   fly  Human    = False

-- |
--
--
instance Move Mammal2 where
  swim x | x == Bat2      = False
         | x == Dolphin2  = True
         | x == Elephant2 = False
         | x == Human2    = True

  walk x | x == Bat2      = False
         | x == Dolphin2  = False
         | x == Elephant2 = True
         | x == Human2    = True

  fly x  | x == Bat2      = True
         | x == Dolphin2  = False
         | x == Elephant2 = False
         | x == Human2    = False


-- |
-- 'Week'       is a `type constructor` if has zero arguments just called a `type`
-- 'Monday',... are `data(value) constructors` if has zero arguments just called a `constant`
type Week :: *
data Week = Sunday
          | Monday
          | Tuesday
          | Wednesday
          | Thursday
          | Friday
          | Saturday

-- |
-- 'Shape'      is a `type constructor` if has zero arguments just called a `type`
-- 'Circle',... are `data(value) constructors` if has zero arguments just called a `constant`
--
type Shape :: *
data Shape = Circle    Float Float Float
           | Rectangle Float Float Float Float

-- |
--
--
someFunc :: IO ()
someFunc = print "Hello, World"
