#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow
-}

import Flow

main :: IO ()
main = putStrLn <| "Hello" <> ", " <> " World!"
