{-# LANGUAGE OverloadedLists #-}

import qualified Data.Map as M

main :: IO ()
main = do
     print $ M.lookup "foo" [("foo", 1), ("bar", 2)]
     print $ M.lookup "foo" (M.fromList [("foo", 1), ("bar", 2)])

