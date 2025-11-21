module Lib
    where

-- |
--
data TypeA = DataA0
     deriving (Show)

data TypeB a = DataB0
             | DataB1 a
     deriving (Show)

data TypeC a = DataC0
             | DataC1 a
             | DataC2 a a
     deriving (Show)

data TypeD a = DataD0
             | DataD1 a
             | DataD2 a a
             | DataD3 a a a
     deriving (Show)

-- |
--
class TypeClassA a where
  typeClassFunctionA :: a -> String

class TypeClassA a => TypeClassB a where
  typeClassFunctionB :: a -> String

class TypeClassB a => TypeClassC a where
  typeClassFunctionC :: a -> String

class TypeClassC a => TypeClassD a where
  typeClassFunctionD :: a -> String

-- |
--
instance TypeClassA TypeA where
  typeClassFunctionA x = "TypeClassA - TypeA"

instance TypeClassB TypeA where
  typeClassFunctionB x = "TypeClassB - TypeA"

instance TypeClassC TypeA where
  typeClassFunctionC x = "TypeClassC - TypeA"

instance TypeClassD TypeA where
  typeClassFunctionD x = "TypeClassD - TypeA"

-- |
--
instance (Num a) => TypeClassA (TypeB a) where
  typeClassFunctionA x = "TypeClassA - TypeB"

instance (Num a) => TypeClassB (TypeB a) where
  typeClassFunctionB x = "TypeClassB - TypeB"

instance (Num a) => TypeClassC (TypeB a) where
  typeClassFunctionC x = "TypeClassC - TypeB"

instance (Num a) => TypeClassD (TypeB a) where
  typeClassFunctionD x = "TypeClassD - TypeB"

-- |
--
instance (Eq a) => TypeClassA (TypeC a) where
  typeClassFunctionA x = "TypeClassA - TypeC"

instance (Eq a) => TypeClassB (TypeC a) where
  typeClassFunctionB x = "TypeClassB - TypeC"

instance (Eq a) => TypeClassC (TypeC a) where
  typeClassFunctionC x = "TypeClassC - TypeC"

instance (Eq a) => TypeClassD (TypeC a) where
  typeClassFunctionD x = "TypeClassD - TypeC"

-- |
--
instance (Ord a) => TypeClassA (TypeD a) where
  typeClassFunctionA x = "TypeClassA - TypeD"

instance (Ord a) => TypeClassB (TypeD a) where
  typeClassFunctionB x = "TypeClassB - TypeD"

instance (Ord a) => TypeClassC (TypeD a) where
  typeClassFunctionC x = "TypeClassC - TypeD"

instance (Ord a) => TypeClassD (TypeD a) where
  typeClassFunctionD x = "TypeClassD - TypeD"

-- |
--
myFunctionA :: TypeA ->  String
myFunctionA x = "TypeA - DataA0"

myFunctionB :: TypeB t ->  String
myFunctionB (DataB1 _) = "TypeB - DataB1"

myFunctionC :: TypeC t ->  String
myFunctionC (DataC1 _)   = "TypeC - DataC1"
myFunctionC (DataC2 _ _) = "TypeC - DataC2"

myFunctionD :: TypeD t ->  String
myFunctionD (DataD1 _)     = "TypeD - DataD1"
myFunctionD (DataD2 _ _)   = "TypeD - DataD2"
myFunctionD (DataD3 _ _ _) = "TypeD - DataD3"

-- |
--
data Car = Car { company :: String
               , model   :: String
               , year    :: Int
               }
     deriving (Show)

data Passenger a b c = Passenger { name   :: a
                                 , gender :: b
                                 , age    :: c
                                 }
     deriving (Show)

-- |
--
tellCar :: Car -> String
tellCar Car {company = c, model = m, year = y} =
  "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

-- |
--
tellPassenger :: (Show t) => Passenger String String t -> String
tellPassenger (Passenger {name = n, gender = g, age = a}) =
  "This person is " ++ n ++ ", " ++ g ++ ", " ++ show a


-- | Given a value
-- | When run `doubleMe`
-- | Then evaluate the result
-- |
--
doubleMe :: (Num a) => a -> a
doubleMe x = x + x

dobuleMe' :: String -> String
dobuleMe' x = x ++ x


-- | Given two values
-- | When run `doubleUs`
-- | Then evaluate the result
-- |
--
doubleUs :: (Num a) => a -> a -> a
doubleUs x y = x*2 + y*2


-- |
--
sum' :: (Num a) => [a] -> a -> a
sum' [] y     = y
sum' [x] y    = sum (x:[y])
sum' (x:xs) y = sum ((sum' [x] y) : xs)


-- |
--
minimum' :: (Ord a) => [a] -> a
minimum' []     = error "Error"
minimum' [x]    = x
minimum' (x:xs) = min x (minimum' xs)

-- |
--
maximum' []     = error "maximum of empty list"
maximum' [x]    = x
maximum' (x:xs) = max x (maximum' xs)


-- |
--
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib(n-2)

-- |
--
hundred :: (Eq a, Num a) => a -> a -> a
hundred _ 0 = 0
hundred x 1 = x
hundred x y = x + (hundred x (y - 1))


-- |
--
someFunc :: IO ()
someFunc = do
--   print $ tellPassenger (Passenger {name="Choo", gender="M", age=40})
--   print $ tellCar (Car {company="For", model="Musting", year=1967})
  let result = doubleUs 1 2 + doubleMe 3
  print result
  return ()
