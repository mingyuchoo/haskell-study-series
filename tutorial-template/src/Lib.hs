module Lib
    ( someFunc
    , MyTypeConstructor(..)
    , MyTypeClass
    , myOtherFunction
    , YesNo
    , yesno
    , yesnoIf
    , TrafficLight(..)
    , Checkable
    , same
    , checkIf
    , Week(..)
    , Shape(..)
    , Mammal(..)
    , Move
    , swim
    , walk
    , fly
    ) where

-- ------------------------------------------------------------------------- --

data MyTypeConstructor = MyDataConstructor deriving (Show)


class MyTypeClass myTypeVariable where
  myTypeClassFunction :: myTypeVariable -> String


instance MyTypeClass MyTypeConstructor where
  myTypeClassFunction MyDataConstructor = "MyValue"


myOtherFunction :: (MyTypeClass myTypeVariable) => myTypeVariable -> String
myOtherFunction x = "MyValue"

-- ------------------------------------------------------------------------- --
-- Class Inheritance (A concerted example)
-- https://en.wikibooks.org/wiki/Haskell/Classes_and_types

-- Location, in two dimesions.
class Located a where
  getLocation :: a -> (Int, Int)


class (Located a) => Movable a where
  setLocation :: (Int, Int) -> a -> a


-- An example type, with accompanying instances.
data NamedPoint = NamedPoint
  { pointName :: String
  , pointX    :: Int
  , pointY    :: Int
  } deriving (Show)



instance Located NamedPoint where
  getLocation p = (pointX p, pointY p)


instance Movable NamedPoint where
  setLocation (x, y) p = p { pointX = x, pointY = y}


-- Moves a value of a Movable type by the specified displacement
-- This works for any movable, including NamedPoint.
move :: (Movable a) => (Int, Int) -> a -> a
move (dx, dy) p = setLocation (x + dx, y + dy) p
  where (x, y) = getLocation p




-- ------------------------------------------------------------------------- --


-- 'TrafficLight' is a `type constructor` if has zero arguments just called a `type`
-- 'Red',...      are `data(value) constructors` if has zero arguments just called a `constant`
--
data TrafficLight = Red | Amber | Green deriving (Eq, Show)


-- 'Checkable' is a `type class` or just a `class`
-- 'a'         is a `type variable`
--
class Checkable a where
    same :: a -> a -> Bool


-- 'Checkable'     is a `type class` or just a `class`
-- 'TrafficLight'  is a `type constructor` if has zero arguments just called a `type` used as a `type variable`
-- 'Red',...       are `data(value) constructors`
--
instance Checkable TrafficLight where
    same Red   Red   = if Red   == Red   then True else False
    same Amber Amber = if Amber == Amber then True else False
    same Green Green = if Green == Green then True else False
    same _     _     = False


-- 'Checkable'     is a `type class constraint` in a function declaration
-- 'a'             is a `type variable`         in a function declaration
checkIf :: (Checkable a) => a -> a -> Bool
checkIf x y  = same x y                         -- 'x', 'y' are `bind variables`

-- ------------------------------------------------------------------------- --

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
  yesno Nothing  = False
  yesno (Just _) = True


instance YesNo TrafficLight where
  yesno Red = False
  yesno _   = True


yesnoIf :: (YesNo a) => a -> b -> b -> b
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

-- >>>>> yesnoIf Red "GO" "STOP" >>> "STOP"

-- ------------------------------------------------------------------------- --

-- 'Mammal'  is a `type constructor` if has zero arguments just called a `type`
-- 'Bat',... are `data(value) constructors` if has zero arguments just called a `constant`
--
data Mammal = Bat | Dolphin | Elephant | Human


-- 'Move' is a `type class` or just a `class`
-- 'a'    is a `type variable`
--
class Move a where
  swim :: a -> Bool
  walk :: a -> Bool
  fly  :: a -> Bool

-- 'Move'    is a `type class` or just a `class`
-- 'Mammal'  is a `type constructor` if has zero arguments just called a `type` used as a `type variable`
-- 'Bat',... are `data(value) constructors`
--
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


-- ------------------------------------------------------------------------- --

-- 'Week'       is a `type constructor` if has zero arguments just called a `type`
-- 'Monday',... are `data(value) constructors` if has zero arguments just called a `constant`
--
data Week = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday


-- ------------------------------------------------------------------------- --

-- 'Shape'      is a `type constructor` if has zero arguments just called a `type`
-- 'Circle',... are `data(value) constructors` if has zero arguments just called a `constant`
--
data Shape = Circle Float Float Float | Rectangle Float Float Float Float


-- ------------------------------------------------------------------------- --

someFunc :: IO ()
someFunc = print "Hello, World"
