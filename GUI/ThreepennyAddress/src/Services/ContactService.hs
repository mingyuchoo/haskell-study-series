module Services.ContactService
    ( ContactService(..)
    , addContact
    , updateContact
    , deleteContact
    , generateNextId
    ) where

import qualified Data.Map as Map
import Models.Contact (Contact(..), ContactId(..))
import Models.AppState (AppState(..))
import Services.ValidationService (ValidationError, validateContactData)

-- | Interface for contact management operations
class ContactService m where
    addContactM :: Contact -> AppState -> m (Either [ValidationError] AppState)
    updateContactM :: Contact -> AppState -> m (Either [ValidationError] AppState)
    deleteContactM :: ContactId -> AppState -> m AppState

-- | Add a new contact to the application state
-- Validates the contact data and assigns a new unique ID
addContact :: Contact -> AppState -> Either [ValidationError] AppState
addContact contactData appState = do
    -- First validate the contact data
    validatedContact <- validateContactData contactData
    
    -- Generate new ID and create contact with that ID
    let newId = nextId appState
        newContact = validatedContact { contactId = newId }
        updatedContacts = Map.insert newId newContact (contacts appState)
        updatedAppState = appState 
            { contacts = updatedContacts
            , nextId = generateNextId newId
            }
    
    return updatedAppState

-- | Update an existing contact while preserving its ID
-- Validates the updated contact data
updateContact :: Contact -> AppState -> Either [ValidationError] AppState
updateContact updatedContact appState = do
    -- Validate the updated contact data
    validatedContact <- validateContactData updatedContact
    
    -- Check if contact exists
    let contactExists = Map.member (contactId validatedContact) (contacts appState)
    
    if contactExists
        then do
            let updatedContacts = Map.insert (contactId validatedContact) validatedContact (contacts appState)
                updatedAppState = appState { contacts = updatedContacts }
            return updatedAppState
        else do
            -- If contact doesn't exist, we could either error or treat as add
            -- For now, we'll just update the map (which will add if not present)
            let updatedContacts = Map.insert (contactId validatedContact) validatedContact (contacts appState)
                updatedAppState = appState { contacts = updatedContacts }
            return updatedAppState

-- | Delete a contact from the application state
-- Returns the updated state with the contact removed
deleteContact :: ContactId -> AppState -> AppState
deleteContact contactIdToDelete appState =
    let updatedContacts = Map.delete contactIdToDelete (contacts appState)
        updatedAppState = appState { contacts = updatedContacts }
    in updatedAppState

-- | Generate the next unique ContactId
generateNextId :: ContactId -> ContactId
generateNextId (ContactId currentId) = ContactId (currentId + 1)