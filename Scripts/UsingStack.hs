#!/usr/bin/env stack
-- stack --resolver lts-24.11 script --package flow

import Flow

main :: IO ()
main = putStrLn <| "Hello" <> ", " <> "World!"
