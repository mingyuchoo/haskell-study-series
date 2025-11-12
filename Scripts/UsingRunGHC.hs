#!/usr/bin/env runghc


-- '''bash
-- $ stack script --resolver lts-24.11 --package flow ./<this-file-name>.hs
-- '''

import Flow

main :: IO ()
main = 
  putStrLn <| "Hello" <> ", " <> "Haskell!"
