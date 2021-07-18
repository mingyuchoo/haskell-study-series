module TypeClassInheritance where

--------------------------------------------------------------------------------

import           Data.Maybe

--------------------------------------------------------------------------------

data TypeA  = DataA0
            | DataA1 { fieldA11 :: String }
            | DataA2 { fieldA21 :: String
                     , fieldA22 :: String }
            deriving (Show)

data TypeB  = DataB0
            | DataB1 { fieldB11 :: String }
            | DataB2 { fieldB21 :: String
                     , fieldB22 :: String }
            deriving (Show)

data TypeC  = DataC0
            | DataC1 { fieldC11 :: String }
            | DataC2 { fieldC21 :: String
                     , fieldC22 :: String }
            deriving (Show)

--------------------------------------------------------------------------------

class TypeClassA a where
    typeClassA :: a -> a

class (TypeClassA a) => TypeClassB a where
    typeClassB :: a -> Maybe a

class (TypeClassB a) => TypeClassC a where
    typeClassC :: a -> Either a a

--------------------------------------------------------------------------------

-- | typeClassA TypeA
--
-- >>> typeClassA DataA0
-- DataA0
--
-- >>> typeClassA DataA1 { fieldA11 = "a11" }
-- DataA1 {fieldA11 = "a11"}
--
-- >>> typeClassA DataA2 { fieldA21 = "a21", fieldA22 = "a22" }
-- DataA2 {fieldA21 = "a21", fieldA22 = "a22"}
--
instance TypeClassA TypeA where
    typeClassA x = x




-- | typeClassA TypeB
--
-- >>> typeClassA DataB0
-- DataB0
--
-- >>> typeClassA DataB1 { fieldB11 = "b11" }
-- DataB1 {fieldB11 = "b11"}
--
-- >>> typeClassA DataB2 { fieldB21 = "b21", fieldB22 = "b22" }
-- DataB2 {fieldB21 = "b21", fieldB22 = "b22"}
--
instance TypeClassA TypeB where
    typeClassA x = x




-- | typeClassA TypeC
--
-- >>> typeClassA DataC0
-- DataC0
--
-- >>> typeClassA DataC1 { fieldC11 = "c11" }
-- DataC1 {fieldC11 = "c11"}
--
-- >>> typeClassA DataC2 { fieldC21 = "c21", fieldC22 = "c22" }
-- DataC2 {fieldC21 = "c21", fieldC22 = "c22"}
--
instance TypeClassA TypeC where
    typeClassA x = x




-- | typeClassB TypeA
--
-- >>> typeClassB DataA0
-- Just DataA0
--
-- >>> typeClassB DataA1 { fieldA11 = "a11" }
-- Just (DataA1 {fieldA11 = "a11"})
--
-- >>> typeClassB DataA2 { fieldA21 = "a21", fieldA22 = "a22" }
-- Just (DataA2 {fieldA21 = "a21", fieldA22 = "a22"})
--
instance TypeClassB TypeA where
    typeClassB x = Just x




-- | typeClassB TypeB
--
-- >>> typeClassB DataB0
-- Just DataB0
--
-- >>> typeClassB DataB1 { fieldB11 = "b11" }
-- Just (DataB1 {fieldB11 = "b11"})
--
-- >>> typeClassB DataB2 { fieldB21 = "b21", fieldB22 = "b22" }
-- Just (DataB2 {fieldB21 = "b21", fieldB22 = "b22"})
--
instance TypeClassB TypeB where
    typeClassB x = Just x




-- | typeClassB TypeC
--
-- >>> typeClassB DataC0
-- Just DataC0
--
-- >>> typeClassB DataC1 { fieldC11 = "c11" }
-- Just (DataC1 {fieldC11 = "c11"})
--
-- >>> typeClassB DataC2 { fieldC21 = "c21", fieldC22 = "c22" }
-- Just (DataC2 {fieldC21 = "c21", fieldC22 = "c22"})
--
instance TypeClassB TypeC where
    typeClassB x = Just x




-- | typeClassC TypeA
--
-- >>> typeClassC DataA0
-- Left DataA0
--
-- >>> typeClassC DataA1 { fieldA11 = "a11" }
-- Left (DataA1 {fieldA11 = "a11"})
--
-- >>> typeClassC DataA2 { fieldA21 = "a21", fieldA22 = "a22" }
-- Left (DataA2 {fieldA21 = "a21", fieldA22 = "a22"})
--
instance TypeClassC TypeA where
    typeClassC x = Left x




-- | typeClassC TypeB
--
-- >>> typeClassC DataB0
-- Left DataB0
--
-- >>> typeClassC DataB1 { fieldB11 = "b11" }
-- Left (DataB1 {fieldB11 = "b11"})
--
-- >>> typeClassC DataB2 { fieldB21 = "b21", fieldB22 = "b22" }
-- Left (DataB2 {fieldB21 = "b21", fieldB22 = "b22"})
--
instance TypeClassC TypeB where
    typeClassC x = Left x




-- | typeClassC TypeC
--
-- >>> typeClassC DataC0
-- Left DataC0
--
-- >>> typeClassC DataC1 { fieldC11 = "c11" }
-- Left (DataC1 {fieldC11 = "c11"})
--
-- >>> typeClassC DataC2 { fieldC21 = "c21", fieldC22 = "c22" }
-- Left (DataC2 {fieldC21 = "c21", fieldC22 = "c22"})
--
instance TypeClassC TypeC where
    typeClassC x = Left x

--------------------------------------------------------------------------------

-- | functionOne
--
-- >>> functionOne DataA0
-- DataA0
--
-- >>> functionOne DataA1 {fieldA11 = "a11"}
-- DataA1 {fieldA11 = "a11"}
--
-- >>> functionOne DataA2 {fieldA21 = "a21", fieldA22 = "a22"}
-- DataA2 {fieldA21 = "a21", fieldA22 = "a22"}
--
-- >>> functionOne DataB0
-- DataB0
--
-- >>> functionOne DataB1 {fieldB11 = "b11"}
-- DataB1 {fieldB11 = "b11"}
--
-- >>> functionOne DataB2 {fieldB21 = "b21", fieldB22 = "b22"}
-- DataB2 {fieldB21 = "b21", fieldB22 = "b22"}
--
-- >>> functionOne DataC0
-- DataC0
--
-- >>> functionOne DataC1 {fieldC11 = "c11"}
-- DataC1 {fieldC11 = "c11"}
--
-- >>> functionOne DataC2 {fieldC21 = "c21", fieldC22 = "c22"}
-- DataC2 {fieldC21 = "c21", fieldC22 = "c22"}
--
functionOne :: (TypeClassA a) => a -> a
functionOne x = x





-- | functionTwo
--
-- >>> functionTwo DataA0
-- DataA0
--
-- >>> functionTwo DataA1 {fieldA11 = "a11"}
-- DataA1 {fieldA11 = "a11"}
--
-- >>> functionTwo DataA2 {fieldA21 = "a21", fieldA22 = "a22"}
-- DataA2 {fieldA21 = "a21", fieldA22 = "a22"}
--
-- >>> functionTwo DataB0
-- DataB0
--
-- >>> functionTwo DataB1 {fieldB11 = "b11"}
-- DataB1 {fieldB11 = "b11"}
--
-- >>> functionTwo DataB2 {fieldB21 = "b21", fieldB22 = "b22"}
-- DataB2 {fieldB21 = "b21", fieldB22 = "b22"}
--
-- >>> functionTwo DataC0
-- DataC0
--
-- >>> functionTwo DataC1 {fieldC11 = "c11"}
-- DataC1 {fieldC11 = "c11"}
--
-- >>> functionTwo DataC2 {fieldC21 = "c21", fieldC22 = "c22"}
-- DataC2 {fieldC21 = "c21", fieldC22 = "c22"}
--
functionTwo :: (TypeClassB a) => a -> a
functionTwo x = x


-- | functionThree
--
-- >>> functionThree DataA0
-- DataA0
--
-- >>> functionThree DataA1 {fieldA11 = "a11"}
-- DataA1 {fieldA11 = "a11"}
--
-- >>> functionThree DataA2 {fieldA21 = "a21", fieldA22 = "a22"}
-- DataA2 {fieldA21 = "a21", fieldA22 = "a22"}
--
-- >>> functionThree DataB0
-- DataB0
--
-- >>> functionThree DataB1 {fieldB11 = "b11"}
-- DataB1 {fieldB11 = "b11"}
--
-- >>> functionThree DataB2 {fieldB21 = "b21", fieldB22 = "b22"}
-- DataB2 {fieldB21 = "b21", fieldB22 = "b22"}
--
-- >>> functionThree DataC0
-- DataC0
--
-- >>> functionThree DataC1 {fieldC11 = "c11"}
-- DataC1 {fieldC11 = "c11"}
--
-- >>> functionThree DataC2 {fieldC21 = "c21", fieldC22 = "c22"}
-- DataC2 {fieldC21 = "c21", fieldC22 = "c22"}
--
functionThree :: (TypeClassC a) => a -> a
functionThree x = x

--------------------------------------------------------------------------------
