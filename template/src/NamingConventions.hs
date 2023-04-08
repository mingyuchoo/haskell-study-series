{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE UndecidableInstances     #-}

module NamingConventions
    where

import           Data.Kind (Constraint, Type)
import           Flow      ((<|))

-- |
--
--
type Dollars :: Type
newtype Dollars = Dollars Int

-- |
--
--
instance (Num Int) => Num Dollars where
  abs (Dollars a) = Dollars (abs a)
  negate (Dollars a) = Dollars (negate a)
  Dollars a + Dollars b = Dollars (a + b)
  Dollars a * Dollars b = Dollars (a * b)
  signum = undefined
  fromInteger = undefined

-- |
--
--
type MyTypeConstructor :: Type -> Type
newtype MyTypeConstructor a = MyDataConstructor a

-- |
-- :kind MyTypeClass :: Type -> Constraint
-- :kind MyTypeClass Int :: Constraint
-- :kind MyTypeClass MyTypeConstructor :: Constraint
-- :type name :: MyTypeClass a => a -> String
type MyTypeClass :: Type -> Constraint
class MyTypeClass a where
  name :: a -> String


-- |
-- :type name :: MyTypeClass a => a -> String
-- :type name (MyDataConstructor "a") :: String
instance (MyTypeClass a) => MyTypeClass (MyTypeConstructor a) where
  name (MyDataConstructor x) = "Instance of MyClassType"

