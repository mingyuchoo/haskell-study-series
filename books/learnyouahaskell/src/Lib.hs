{-# OPTIONS_GHC -fwarn-missing-signatures #-}

{-# LANGUAGE ExplicitForAll           #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Lib
    where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
