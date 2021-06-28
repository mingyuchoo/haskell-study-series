module Lib where

-- ------------------------------------------------------------------------- --

data TypeA   = DataA1                               deriving (Show)
data TypeB a = DataB1 a                             deriving (Show)
data TypeC a = DataC1 a | DataC2 a a                deriving (Show)
data TypeD a = DataD1 a | DataD2 a a | DataD3 a a a deriving (Show)



class TypeClassA a where
  typeClassFunctionA :: a -> String

class TypeClassA a => TypeClassB a where
  typeClassFunctionB :: a -> String

class TypeClassB a => TypeClassC a where
  typeClassFunctionC :: a -> String

class TypeClassC a => TypeClassD a where
  typeClassFunctionD :: a -> String



instance TypeClassA TypeA where
  typeClassFunctionA x = "ValueA"

instance TypeClassB TypeA where
  typeClassFunctionB x = "ValueA"

-- instance TypeClassC TypeC where
--   typeClassFunctionC x = "ValueC"
--
-- instance TypeClassD TypeD where
--   typeClassFunctionD x = "ValueD"




myFunctionA :: TypeA ->  String
myFunctionA x  = "TypeA - DataA"

myFunctionB :: TypeB t ->  String
myFunctionB (DataB1 _)  = "TypeB - DataB1"

myFunctionC :: TypeC t ->  String
myFunctionC (DataC1 _)   = "TypeC - DataC1"
myFunctionC (DataC2 _ _) = "TypeC - DataC2"

myFunctionD :: TypeD t ->  String
myFunctionD (DataD1 _)     = "TypeD - DataD1"
myFunctionD (DataD2 _ _)   = "TypeD - DataD2"
myFunctionD (DataD3 _ _ _) = "TypeD - DataD3"


-- ------------------------------------------------------------------------ --
data Car  = Car { company :: String
                , model   :: String
                , year    :: Int
} deriving (Show)

data Passenger a b c  = Passenger { name :: a
                           , gender      :: b
                           , age         :: c
} deriving (Show)

-- ------------------------------------------------------------------------ --

-- Car
tellCar :: Car -> String
tellCar Car {company = c, model = m, year = y} =
  "This " ++ c ++ " " ++ m ++ " was made in " ++ show y


-- Passenger
tellPassenger :: (Show t) => Passenger String String t -> String
tellPassenger (Passenger {name = n, gender = g, age = a}) =
  "This person is " ++ n ++ ", " ++ g ++ ", " ++ show a


-- | Given a value
-- | When run `doubleMe`
-- | Then evaluate the result

doubleMe :: (Num a) => a -> a
doubleMe x = x + x

dobuleMe' :: String -> String
dobuleMe' x = x ++ x


-- | Given two values
-- | When run `doubleUs`
-- | Then evaluate the result

doubleUs :: (Num a) => a -> a -> a
doubleUs x y = x*2 + y*2






-- ------------------------------------------------------------------------ --
someFunc :: IO ()
someFunc = do
--   print $ tellPassenger (Passenger {name="Choo", gender="M", age=40})
--   print $ tellCar (Car {company="For", model="Musting", year=1967})
  let result = doubleUs 1 2 + doubleMe 3
  print result
  return ()
