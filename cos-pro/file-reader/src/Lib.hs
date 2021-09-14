module Lib
    ( someFunc
    ) where

import           Data.List
import           System.Directory      (canonicalizePath, getDirectoryContents)
import           System.Environment    (getArgs)
import           System.FilePath.Posix ((</>))

getAbsDirectoryContents :: FilePath -> IO [FilePath]
getAbsDirectoryContents dir =
    getDirectoryContents dir >>= mapM (canonicalizePath . (dir </>))


someFunc :: IO ()
someFunc = do
    args <- getArgs
    print args
