-- | Compatibility facade. Runtime owns atomic execution; adapters own IO resources.
module MyOrg.Store (Store, openFileStore, openPostgresStore, closeStore, readStore, readRegistry, readAudit, runCommand, runOrganizationCommand, seedDemo) where

import Control.Exception (mask_)
import qualified Data.ByteString.Char8 as BS
import MyOrg.Application.Runtime
import MyOrg.Infrastructure.FileStore (openFilePersistence)
import MyOrg.Infrastructure.PostgresStore (openPostgresPersistence)

openFileStore :: FilePath -> IO Store
openFileStore path = mask_ (openFilePersistence path >>= openStore)

openPostgresStore :: BS.ByteString -> IO Store
openPostgresStore connectionString = mask_ (openPostgresPersistence connectionString >>= openStore)
