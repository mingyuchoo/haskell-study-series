module Services.ContactRepository
    ( ContactRepository(..)
    , FileContactRepository(..)
    , loadContactsFromFile
    , saveContactsToFile
    ) where

import Control.Exception (IOException, try)
import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import Data.List (sortOn)
import Models.AppState (AppState(..))
import Models.Contact (Contact(..), ContactId(..))
import System.Directory (doesFileExist)
import qualified Data.Map as Map

-- | Interface for contact data persistence
class ContactRepository m where
    loadContacts :: m (Either String [Contact])
    saveContacts :: [Contact] -> m (Either String ())

-- | File-based implementation of ContactRepository
data FileContactRepository = FileContactRepository
    { repositoryFilePath :: FilePath
    } deriving (Show, Eq)

-- | Load contacts from JSON file
loadContactsFromFile :: FilePath -> IO (Either String [Contact])
loadContactsFromFile filePath = do
    fileExists <- doesFileExist filePath
    if not fileExists
        then return $ Right []  -- Return empty list if file doesn't exist
        else do
            result <- try $ eitherDecodeFileStrict filePath
            case result of
                Left (ioErr :: IOException) -> 
                    return $ Left $ "IO Error reading file: " ++ show ioErr
                Right (Left jsonErr) -> 
                    return $ Left $ "JSON parsing error: " ++ jsonErr
                Right (Right appState) -> 
                    return $ Right $ sortOn contactId $ Map.elems (contacts appState)

-- | Save contacts to JSON file
saveContactsToFile :: FilePath -> [Contact] -> IO (Either String ())
saveContactsToFile filePath contactList = do
    let contactMap = Map.fromList [(contactId c, c) | c <- contactList]
    let maxId = if null contactList 
                then ContactId 1 
                else ContactId $ (\(ContactId i) -> i + 1) $ maximum $ map contactId contactList
    let appState = AppState
            { contacts = contactMap
            , nextId = maxId
            , searchTerm = ""
            }
    result <- try $ encodeFile filePath appState
    case result of
        Left (ioErr :: IOException) -> 
            return $ Left $ "IO Error writing file: " ++ show ioErr
        Right () -> 
            return $ Right ()

-- | Instance for IO monad
instance ContactRepository IO where
    loadContacts = loadContactsFromFile "contacts.json"
    saveContacts = saveContactsToFile "contacts.json"