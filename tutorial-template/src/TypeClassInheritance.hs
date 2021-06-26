module TypeClassInheritance
    ( TypeClassA
    , TypeClassB
    , TypeA(..)
    , TypeB(..)
    , TypeC(..)
    , functionOne
    , functionTwo
    , functionThree
    ) where

import           Data.Maybe

class TypeClassA a where
    typeClassA :: a -> String

class (TypeClassA a) => TypeClassB a where
    typeClassB :: a -> Maybe String

class (TypeClassB a) => TypeClassC a where
    typeClassC :: a -> Either String String


data TypeA  = DataA1 | DataA2 | DataA3 deriving (Show)
data TypeB  = DataB1 | DataB2 | DataB3 deriving (Show)
data TypeC  = DataC1 | DataC2 | DataC3 deriving (Show)


instance TypeClassA TypeA where
    typeClassA x = "TypeA"

instance TypeClassB TypeA where
    typeClassB x = Just "TypeA"

instance TypeClassC TypeA where
    typeClassC x = Left "TypeA"





instance TypeClassA TypeB where
    typeClassA x = "TypeB"

instance TypeClassB TypeB where
    typeClassB x = Just "TypeB"

instance TypeClassC TypeB where
    typeClassC x = Left "TypeB"





instance TypeClassA TypeC where
    typeClassA x = "TypeC"

instance TypeClassB TypeC where
    typeClassB x = Just "TypeC"

instance TypeClassC TypeC where
    typeClassC x = Left "TypeC"





functionOne :: (TypeClassA a) => a -> String
functionOne x = "TypeA"


functionTwo :: (TypeClassB a) => a -> String
functionTwo x = "TypeA"


functionThree :: (TypeClassC a) => a -> String
functionThree x = "TypeA"
