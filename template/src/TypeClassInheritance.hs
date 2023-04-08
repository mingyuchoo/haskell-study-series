
module TypeClassInheritance
    where


import           Data.Kind  (Constraint, Type)
import           Data.Maybe


-- 1. Algebraic Data Type (ADT)
-- 2. Parametric Pohlymorphism
-- 3. Data Type declaration


-- `TypeA` is a Data Type
-- (o) :kind TypeA    :: Type
-- (o) :type DataA0   :: TypeA
-- (o) :type DataA1   :: String -> TypeA
-- (x) :type DataA1 String  <<error>>
-- (o) :type DataA1 "A1" :: TypeA
-- (o) :type fieldA11 :: TypeA -> String
type TypeA :: Type
data TypeA  = DataA0                         -- `DataA0` is a Data Constructor
            | DataA1 { fieldA11 :: String }  -- `DataA1` is a Data Constructor
            deriving (Show)


-- `TypeB a` is a Data Type having `a` type variable
-- (o) :kind TypeB    :: Type -> Type
-- (o) :type DataB0   :: TypeB a
-- (o) :type DataB1   :: String -> TypeB a
-- (o) :type DataB2   :: a -> a -> TypeB a
-- (o) :type fieldB11 :: TypeB a -> String
-- (o) :type fieldB21 :: TypeB a -> a
-- (o) :type fieldB22 :: TypeB a -> a
type TypeB :: Type -> Type
data TypeB a = DataB0                        -- `DataB0` is a Data Constructor
             | DataB1 { fieldB11 :: String } -- `DataB1` is a Data Constructor
             | DataB2 { fieldB21 :: a        -- `a` is a type variable but when fieldB22 is called,
                      , fieldB22 :: a }      --   `a` is used to a value of the type
             deriving (Show)


-- `TypeC a b` is a Data Type having `a and b` type variables
-- (o) :kind TypeC  :: Type -> Type -> Type
-- (o) :type DataC0 :: Type C a b
-- (o) :type DataC1 :: String -> TypeC a b
-- (o) :type DataC2 :: String -> a -> TypeC a b
-- (o) :type DataC3 :: String -> b -> b -> TypeC a b
-- (o) :type DataC4 :: String -> a -> b -> TypeC a b
-- (o) :type fieldC11 :: TypeC a b -> String
-- (o) :type fieldC21 :: TypeC a b -> String
-- (o) :type fieldC22 :: TypeC a b -> a
-- (o) :type fieldC31 :: TypeC a b -> String
-- (o) :type fieldC32 :: TypeC a b -> b
-- (o) :type fieldC33 :: TypeC a b -> b
-- (o) :type fieldC41 :: TypeC a b -> String
-- (o) :type fieldC42 :: TypeC a b -> a
-- (o) :type fieldC43 :: TypeC a b -> b
type TypeC :: Type -> Type -> Type
data TypeC a b = DataC0
               | DataC1 { fieldC11 :: String }
               | DataC2 { fieldC21 :: String
                        , fieldC22 :: a }
               | DataC3 { fieldC31 :: String
                        , fieldC32 :: b
                        , fieldC33 :: b }
               | DataC4 { fieldC41 :: String
                        , fieldC42 :: a
                        , fieldC43 :: b }
               deriving (Show)


-- 1. Ad-hoc Polymorphism
-- 2. Type Class Inheritance
-- 3. Type Class declaration


-- `TypeCassA a` is a Type Class having `a` type variable
-- (o) :kind TypeClassA :: Type -> Constraint
-- (o) :kind TypeClassA Char :: Constraint
-- (o) :kind TypeClassA Int  :: Constraint
-- (o) :kind TypeClassA TypeA :: Constraint
-- (o) :kind TypeClassA (TypeB Int) :: Constraint
-- (o) :kind TypeClassA (TypeC Int String) :: Constraint
-- (o) :type functionA :: TypeClassA a => a -> a
type TypeClassA :: Type -> Constraint
class TypeClassA a where                   -- `a` is a type variable
    functionA :: a -> a


-- `TypeCassB b` is a Type Class having `b` type variable which inheriting `TypeClassA b`
-- (o) :kind TypeClassB :: Type -> Constraint
-- (o) :kind TypeClassB Char :: Constraint
-- (o) :kind TypeClassB Int  :: Constraint
-- (o) :kind TypeClassB TypeA :: Constraint
-- (o) :kind TypeClassB (TypeB Int) :: Constraint
-- (o) :kind TypeClassB (TypeC Int String) :: Constraint
-- (o) :type functionB :: TypeClassB b => b -> Maybe b
type TypeClassB :: Type -> Constraint
class (TypeClassA b) => TypeClassB b where -- `b` is a type variable
    functionB :: b -> Maybe b


-- `TypeCassC c` is a Type Class having `c` type variable which inheriting `TypeClassB c`
-- (o) :kind TypeClassC :: Type -> Constraint
-- (o) :kind TypeClassC Char :: Constraint
-- (o) :kind TypeClassC Int  :: Constraint
-- (o) :kind TypeClassC TypeA :: Constraint
-- (o) :kind TypeClassC (TypeB Int) :: Constraint
-- (o) :kind TypeClassC (TypeC Int String) :: Constraint
-- (o) :type functionC :: TypeClassC c => c -> Either c c
type TypeClassC :: Type -> Constraint
class (TypeClassB c) => TypeClassC c where -- `c` is a type variable
    functionC :: c -> Either c c



-- | functionA TypeA
--
-- >>> functionA DataA0
-- DataA0
--
-- >>> functionA (DataA1 "a11")
-- DataA1 {fieldA11 = "a11"}
--
-- >>> functionA $ DataA1 "a11"
-- DataA1 {fieldA11 = "a11"}
--
-- >>> functionA DataA1 { fieldA11 = "a11" }
-- DataA1 {fieldA11 = "a11"}
--
instance TypeClassA TypeA where  -- `TypeA` is a Data Type
    functionA x = x              -- `x` is a value paramter for a type variable
                                 -- when call `functionA`, `x` should be
                                 --   a data or a value


-- | functionA TypeB
--
-- >>> functionA DataB0
-- DataB0
--
-- >>> functionA DataB1 { fieldB11 = "b11" }
-- DataB1 {fieldB11 = "b11"}
--
-- >>> functionA DataB2 { fieldB21 = "b21", fieldB22 = "b22" }
-- DataB2 {fieldB21 = "b21", fieldB22 = "b22"}
--
instance TypeClassA (TypeB a) where -- `(TypeB a)` is a Data Type having a type variable
    functionA x = x                 -- `x` is a value paramter for a type variable
                                    -- when call `functionA`, `x` should be
                                    --   a data or a value


-- | functionA TypeC
--
-- >>> functionA DataC0
-- DataC0
--
-- >>> functionA DataC1 { fieldC11 = "c11" }
-- DataC1 {fieldC11 = "c11"}
--
-- >>> functionA DataC2 { fieldC21 = "c21", fieldC22 = "c22" }
-- DataC2 {fieldC21 = "c21", fieldC22 = "c22"}
--
instance TypeClassA (TypeC a b) where -- `(TypeC a b) is a Data Type having two type variables
    functionA x = x                   -- `x` is a value parameter for a type variable
                                      -- when call `functionA`, `x` should be
                                      --   a data or a value




-- | functionB TypeA
--
-- >>> functionB DataA0
-- Just DataA0
--
-- >>> functionB DataA1 { fieldA11 = "a11" }
-- Just (DataA1 {fieldA11 = "a11"})
--
instance TypeClassB TypeA where -- `TypeA` is a Data Type
    functionB x = Just x        -- `x` is a value parameter for a type variable
                                -- when call `functionA`, `x` should be
                                --   a data or a value




-- | functionB TypeB
--
-- >>> functionB DataB0
-- Just DataB0
--
-- >>> functionB DataB1 { fieldB11 = "b11" }
-- Just (DataB1 {fieldB11 = "b11"})
--
-- >>> functionB DataB2 { fieldB21 = "b21", fieldB22 = "b22" }
-- Just (DataB2 {fieldB21 = "b21", fieldB22 = "b22"})
--
instance TypeClassB (TypeB a) where -- `(TypeB a)` is a Data Type having a type variable
    functionB x = Just x            -- `x` is a value parameter for a type variable
                                    -- when call `functionA`, `x` should be
                                    --   a data or a value



-- | functionB TypeC
--
-- >>> functionB DataC0
-- Just DataC0
--
-- >>> functionB DataC1 { fieldC11 = "c11" }
-- Just (DataC1 {fieldC11 = "c11"})
--
-- >>> functionB DataC2 { fieldC21 = "c21", fieldC22 = "c22" }
-- Just (DataC2 {fieldC21 = "c21", fieldC22 = "c22"})
--
instance TypeClassB (TypeC a b) where -- `(TypeC a b)` is a Data Type having two type variables
    functionB x = Just x              -- `x` is a value paramter for a type variable
                                      -- when call `functionA`, `x` should be
                                      --   a data or a value




-- | functionC TypeA
--
-- >>> functionC DataA0
-- Left DataA0
--
-- >>> functionC DataA1 { fieldA11 = "a11" }
-- Left (DataA1 {fieldA11 = "a11"})
--
instance TypeClassC TypeA where -- `TypeA` is a Data Type
    functionC x = Left x        -- `x` is a value parameter for a type variable
                                -- when call `functionA`, `x` should be
                                --   a data or a value




-- | functionC TypeB
--
-- >>> functionC DataB0
-- Left DataB0
--
-- >>> functionC DataB1 { fieldB11 = "b11" }
-- Left (DataB1 {fieldB11 = "b11"})
--
-- >>> functionC DataB2 { fieldB21 = "b21", fieldB22 = "b22" }
-- Left (DataB2 {fieldB21 = "b21", fieldB22 = "b22"})
--
instance TypeClassC (TypeB a) where -- `(TypeB a)` is a Data Type having a type variable
    functionC x = Left x            -- `x` is a value parameter for a type variable
                                    -- when call `functionA`, `x` should be
                                    --   a data or a value



-- | functionC TypeC
--
-- >>> functionC DataC0
-- Left DataC0
--
-- >>> functionC DataC1 { fieldC11 = "c11" }
-- Left (DataC1 {fieldC11 = "c11"})
--
-- >>> functionC DataC2 { fieldC21 = "c21", fieldC22 = "c22" }
-- Left (DataC2 {fieldC21 = "c21", fieldC22 = "c22"})
--
instance TypeClassC (TypeC a b) where -- `(TypeC a b)` is a Data Type having two type variables
    functionC x = Left x              -- `x` is a value parameter for a type variable
                                      -- when call `functionA`, `x` should be
                                      --   a data or a value



-- | functionOne
--
-- >>> functionOne DataA0
-- DataA0
--
-- >>> functionOne DataA1 {fieldA11 = "a11"}
-- DataA1 {fieldA11 = "a11"}
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
-- (o) :type functionOne DataA0 :: TypeA
-- (o) :type functionOne DataB0 :: TypeB a
-- (o) :type functionOne DataC0 :: TypeC a b
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
-- (o) :type functionTwo DataA0 :: TypeA
-- (o) :type functionTwo DataB0 :: TypeB a
-- (o) :type functionTwo DataC0 :: TypeC a b
functionTwo :: (TypeClassB b) => b -> b
functionTwo x = x


-- | functionThree
--
-- >>> functionThree DataA0
-- DataA0
--
-- >>> functionThree DataA1 {fieldA11 = "a11"}
-- DataA1 {fieldA11 = "a11"}
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
-- (o) :type functionThree DataA0 :: TypeA
-- (o) :type functionThree DataB0 :: TypeB a
-- (o) :type functionThree DataC0 :: TypeC a b
functionThree :: (TypeClassC c) => c -> c
functionThree x = x


