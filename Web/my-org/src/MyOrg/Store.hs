-- | Compatibility facade. Runtime owns atomic execution; adapters own IO resources.
module MyOrg.Store
  ( Store
  , openFileStore
  , openSQLiteStore
  , closeStore
  , readStore
  , readRegistry
  , readAudit
  , runCommand
  , runOrganizationCommand
  , seedDemo
  ) where

import Control.Exception (mask_)
import MyOrg.Application.Runtime
import MyOrg.Infrastructure.FileStore (openFilePersistence)
import MyOrg.Infrastructure.SQLiteStore (openSQLitePersistence)

openFileStore :: FilePath -> IO Store
openFileStore path = mask_ (openFilePersistence path >>= openStore)

openSQLiteStore :: FilePath -> IO Store
openSQLiteStore path = mask_ (openSQLitePersistence path >>= openStore)
