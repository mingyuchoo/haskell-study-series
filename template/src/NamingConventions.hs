{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE UndecidableInstances     #-}

module NamingConventions
    where

import           Data.Kind (Constraint, Type)
import           Flow      ((<|))

-- |
--
--
type Dollars :: *
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
type MyTypeConstructor :: * -> *
newtype MyTypeConstructor a = MyDataConstructor a

-- |
-- :kind MyTypeClass :: * -> Constraint
-- :kind MyTypeClass Int :: Constraint
-- :kind MyTypeClass MyTypeConstructor :: Constraint
-- :type name :: MyTypeClass a => a -> String
type MyTypeClass :: * -> Constraint
class MyTypeClass a where
  name :: a -> String


-- |
-- :type name :: MyTypeClass a => a -> String
-- :type name (MyDataConstructor "a") :: String
instance (MyTypeClass a) => MyTypeClass (MyTypeConstructor a) where
  name (MyDataConstructor x) = "Instance of MyClassType"

