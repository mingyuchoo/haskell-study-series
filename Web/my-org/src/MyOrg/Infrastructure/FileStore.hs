-- | Atomic JSON file replacement with an exclusive directory lock.
module MyOrg.Infrastructure.FileStore
  ( openFilePersistence
  ) where

import Control.Exception (bracketOnError, onException)
import Data.ByteString.Lazy qualified as BL
import MyOrg.Application.Persistence
import MyOrg.Domain.Event.Types
import MyOrg.Serialization.Persistence (decodeStoredEvents, encodeStoredEvents)
import System.Directory
import System.IO (hClose, openBinaryTempFile)

openFilePersistence :: FilePath -> IO Persistence
openFilePersistence path = do
  let dir = case reverse (dropWhile (/= '/') (reverse path)) of "" -> "."; d -> d
      lock = path <> ".lock"
  createDirectoryIfMissing True dir
  createDirectory lock
  ( do
      exists <- doesFileExist path
      events <- if exists then BL.readFile path >>= decodeEvents else pure []
      let persist xs = bracketOnError
            (openBinaryTempFile dir ".my-org-events.tmp")
            (\(tmp, h) -> hClose h >> removeFile tmp)
            $ \(tmp, h) -> do
              BL.hPut h (encodeStoredEvents xs)
              hClose h
              renameFile tmp path
      pure (Persistence events persist (removeDirectory lock))
    )
    `onException` removeDirectory lock

decodeEvents :: BL.ByteString -> IO [StoredEvent]
decodeEvents bytes =
  either (ioError . userError . ("Corrupt event file: " <>)) pure (decodeStoredEvents bytes)
