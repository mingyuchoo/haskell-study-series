module Chapter08.MakingOurOwnTypesAndTypeclasses
(
) where

data Bool = False | True
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- | area
area :: Shape -> Float  -- function signature declaration
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)


-- | nudge
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))


-- | baseCircle
baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r


-- | baseRect
baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

data Person = Person String String Int Float String String deriving (Show)

{--
-- | firstName
firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstname

-- | lastname
lastName :: Person -> String
lastName (Person _ lastname _ _ _ _) = lastname

-- | age
age :: Person -> Int
age (Person _ _ age _ _ _) = age

-- | height
height :: Person -> Float
height (Person _ _ _ height _ _) = height

-- | phoneNumber
phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ number _) = number

-- | flavor
flavor :: Person -> String
flavor (Person _ _ _ _ _ flavor) = flavor

--}

data Person2 = Person2 { firstName :: String
                       , lastName :: String
											 , age :: Int
											 , height :: Float
											 , phoneNumber :: String
											 , flavor :: String
} deriving (Show)


-- data Car = Car { company :: String
--                , model :: String
-- 							 , year :: Int
-- } deriving (Show)

-- tellCar :: Car -> String
-- tellCar (Car { company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data Maybe a = Nothing | Just a deriving (Show)


data Car2 a b c = Car2 { company :: a
               , model :: b
							 , year :: c
} deriving (Show)

tellCar2 :: (Show a) => Car2 String String a -> String
tellCar2 (Car2 {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y


data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i+m) (j+m) (k*m)

