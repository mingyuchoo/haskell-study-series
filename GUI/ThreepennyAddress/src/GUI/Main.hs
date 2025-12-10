{-# LANGUAGE RecursiveDo #-}
module GUI.Main
    ( startAddressBookGUI
    , setupGUI
    , AppStateManager(..)
    , createAppStateManager
    , updateAppState
    , getAppState
    , KeyboardShortcut(..)
    , handleKeyboardShortcut
    , parseKeyboardShortcut
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, readTVar, writeTVar, atomically)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, forM_, when)
import Data.Map qualified as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Models.AppState (AppState(..))
import Models.Contact (ContactId(..), Contact(..))
import Services.ContactService (addContact, updateContact, deleteContact)
import Services.SearchService (searchContacts)
import Services.ValidationService (ValidationError(..))
import Services.ContactRepository (loadContactsFromFile, saveContactsToFile)

-- | Keyboard shortcuts supported by the application
data KeyboardShortcut
    = NewContact      -- ^ Ctrl+N: Open new contact form
    | DeleteContact   -- ^ Delete key: Delete selected contact
    | CancelOperation -- ^ Escape: Cancel current operation
    | FocusSearch     -- ^ Ctrl+F: Focus search input
    deriving (Show, Eq)

-- | Handle a keyboard shortcut and return the corresponding action name
handleKeyboardShortcut :: KeyboardShortcut -> String
handleKeyboardShortcut NewContact = "new-contact"
handleKeyboardShortcut DeleteContact = "delete-contact"
handleKeyboardShortcut CancelOperation = "cancel-operation"
handleKeyboardShortcut FocusSearch = "focus-search"

-- | Parse keyboard event data into a KeyboardShortcut
-- Takes key code, ctrlKey flag, and returns Maybe KeyboardShortcut
parseKeyboardShortcut :: Int -> Bool -> Maybe KeyboardShortcut
parseKeyboardShortcut keyCode ctrlKey
    | ctrlKey && keyCode == 78  = Just NewContact      -- Ctrl+N (N = 78)
    | ctrlKey && keyCode == 70  = Just FocusSearch     -- Ctrl+F (F = 70)
    | keyCode == 46             = Just DeleteContact   -- Delete key = 46
    | keyCode == 27             = Just CancelOperation -- Escape = 27
    | otherwise                 = Nothing

-- | Application state manager using TVar for thread-safe state management
data AppStateManager = AppStateManager
    { appStateTVar :: TVar AppState
    }

-- | Create a new application state manager with initial empty state
createAppStateManager :: IO AppStateManager
createAppStateManager = do
    let initialState = AppState
            { contacts = Map.empty
            , nextId = ContactId 1
            , searchTerm = T.empty
            }
    tvar <- newTVarIO initialState
    return $ AppStateManager tvar

-- | Update the application state using STM
updateAppState :: AppStateManager -> (AppState -> AppState) -> IO ()
updateAppState (AppStateManager tvar) updateFn = 
    atomically $ do
        currentState <- readTVar tvar
        writeTVar tvar (updateFn currentState)

-- | Get the current application state
getAppState :: AppStateManager -> IO AppState
getAppState (AppStateManager tvar) = readTVarIO tvar

-- | Set the application state directly
setAppState :: AppStateManager -> AppState -> IO ()
setAppState (AppStateManager tvar) newState = 
    atomically $ writeTVar tvar newState

-- | Data file path
contactsFilePath :: FilePath
contactsFilePath = "contacts.json"

-- | Start the Threepenny-GUI application
startAddressBookGUI :: IO ()
startAddressBookGUI = do
    putStrLn "Starting Threepenny-GUI Address Book..."
    startGUI' defaultConfig
        { jsPort       = Just 8023
        , jsStatic     = Just "static"
        , jsCustomHTML = Just "index.html"
        }

-- | Internal function to start GUI with configuration
startGUI' :: Config -> IO ()
startGUI' config = do
    putStrLn "Server starting on http://localhost:8023"
    Graphics.UI.Threepenny.Core.startGUI config setupGUI

-- | Form mode: Add or Edit
data FormMode = AddMode | EditMode ContactId
    deriving (Show, Eq)


-- | Set up the main GUI application
setupGUI :: Window -> UI ()
setupGUI window = do
    return window # set title "Address Book"
    
    -- Create application state manager
    stateManager <- liftIO createAppStateManager
    
    -- Load existing contacts from file
    loadResult <- liftIO $ loadContactsFromFile contactsFilePath
    case loadResult of
        Right contactList -> do
            let contactMap = Map.fromList [(contactId c, c) | c <- contactList]
            let maxId = if null contactList 
                        then ContactId 1 
                        else ContactId $ (\(ContactId i) -> i + 1) $ maximum $ map contactId contactList
            liftIO $ setAppState stateManager $ AppState
                { contacts = contactMap
                , nextId = maxId
                , searchTerm = T.empty
                }
        Left _ -> return ()  -- Start with empty state on error
    
    -- Create main container
    container <- UI.div # set UI.style 
        [ ("max-width", "900px")
        , ("margin", "0 auto")
        , ("padding", "20px")
        , ("font-family", "Arial, sans-serif")
        ]
    
    -- Create header
    header <- UI.h1 # set text "Address Book"
                   # set UI.style [("text-align", "center"), ("color", "#333")]
    
    -- Create search section
    searchLabel <- UI.label # set text "Search contacts:"
    searchInput <- UI.input # set UI.type_ "text"
                           # set (UI.attr "placeholder") "Enter name, phone, or email..."
                           # set UI.style [("width", "100%"), ("padding", "8px"), ("margin", "5px 0"), ("box-sizing", "border-box")]
    searchDiv <- UI.div # set UI.style [("margin-bottom", "20px")]
    element searchDiv #+ [element searchLabel, UI.br, element searchInput]
    
    -- Create action buttons
    addButton <- createButton "Add Contact" "#4CAF50"
    editButton <- createButton "Edit Contact" "#2196F3"
    deleteButton <- createButton "Delete Contact" "#f44336"
    actionDiv <- UI.div # set UI.style [("margin-bottom", "20px")]
    element actionDiv #+ [element addButton, element editButton, element deleteButton]
    
    -- Create contact list section
    listHeader <- UI.h3 # set text "Contacts"
    contactTableBody <- mkElement "tbody" # set UI.style []
    contactTable <- UI.table # set UI.style 
        [ ("width", "100%")
        , ("border-collapse", "collapse")
        , ("border", "1px solid #ddd")
        ]
    
    -- Create table header
    headerRow <- UI.tr
    selectHeader <- UI.th # set text "" 
                         # set UI.style [("border", "1px solid #ddd"), ("padding", "8px"), ("background-color", "#f2f2f2"), ("width", "40px")]
    nameHeader <- UI.th # set text "Name" 
                       # set UI.style [("border", "1px solid #ddd"), ("padding", "8px"), ("background-color", "#f2f2f2")]
    phoneHeader <- UI.th # set text "Phone"
                        # set UI.style [("border", "1px solid #ddd"), ("padding", "8px"), ("background-color", "#f2f2f2")]
    emailHeader <- UI.th # set text "Email"
                        # set UI.style [("border", "1px solid #ddd"), ("padding", "8px"), ("background-color", "#f2f2f2")]
    addressHeader <- UI.th # set text "Address"
                          # set UI.style [("border", "1px solid #ddd"), ("padding", "8px"), ("background-color", "#f2f2f2")]
    actionsHeader <- UI.th # set text "Actions"
                          # set UI.style [("border", "1px solid #ddd"), ("padding", "8px"), ("background-color", "#f2f2f2"), ("width", "100px")]
    
    tableHead <- mkElement "thead" # set UI.style []
    element headerRow #+ [element selectHeader, element nameHeader, element phoneHeader, element emailHeader, element addressHeader, element actionsHeader]
    element tableHead #+ [element headerRow]
    element contactTable #+ [element tableHead, element contactTableBody]
    
    emptyMessage <- UI.p # set text "No contacts found. Click 'Add Contact' to get started."
                        # set UI.style [("text-align", "center"), ("color", "#666"), ("font-style", "italic")]
    
    listDiv <- UI.div # set UI.style [("margin-bottom", "20px")]
    element listDiv #+ [element listHeader, element contactTable, element emptyMessage]
    
    -- Create error message display
    errorDiv <- UI.div # set UI.style [("display", "none"), ("color", "#f44336"), ("padding", "10px"), ("background-color", "#ffebee"), ("border-radius", "4px"), ("margin-bottom", "10px")]
    
    -- Create contact form section (initially hidden)
    formHeader <- UI.h3 # set text "Add Contact"
    nameLabel <- UI.label # set text "Name (required):"
    nameInput <- UI.input # set UI.type_ "text"
                         # set UI.style [("width", "100%"), ("padding", "8px"), ("margin", "5px 0 10px 0"), ("box-sizing", "border-box")]
    phoneLabel <- UI.label # set text "Phone:"
    phoneInput <- UI.input # set UI.type_ "tel"
                          # set UI.style [("width", "100%"), ("padding", "8px"), ("margin", "5px 0 10px 0"), ("box-sizing", "border-box")]
    emailLabel <- UI.label # set text "Email:"
    emailInput <- UI.input # set UI.type_ "email"
                          # set UI.style [("width", "100%"), ("padding", "8px"), ("margin", "5px 0 10px 0"), ("box-sizing", "border-box")]
    addressLabel <- UI.label # set text "Address:"
    addressInput <- UI.textarea # set UI.style [("width", "100%"), ("padding", "8px"), ("margin", "5px 0 10px 0"), ("height", "60px"), ("box-sizing", "border-box")]
    
    saveButton <- createButton "Save" "#4CAF50"
    cancelButton <- createButton "Cancel" "#666"
    buttonDiv <- UI.div
    element buttonDiv #+ [element saveButton, element cancelButton]
    
    formDiv <- UI.div # set UI.style [("display", "none"), ("border", "1px solid #ddd"), ("padding", "20px"), ("background-color", "#f9f9f9"), ("border-radius", "4px"), ("margin-bottom", "20px")]
    element formDiv #+ 
        [ element formHeader
        , element errorDiv
        , element nameLabel, UI.br, element nameInput
        , element phoneLabel, UI.br, element phoneInput
        , element emailLabel, UI.br, element emailInput
        , element addressLabel, UI.br, element addressInput
        , element buttonDiv
        ]
    
    -- Create delete confirmation dialog (initially hidden)
    confirmHeader <- UI.h3 # set text "Confirm Delete"
    confirmMessage <- UI.p # set text "Are you sure you want to delete this contact?"
    confirmYesButton <- createButton "Yes, Delete" "#f44336"
    confirmNoButton <- createButton "Cancel" "#666"
    confirmButtonDiv <- UI.div
    element confirmButtonDiv #+ [element confirmYesButton, element confirmNoButton]
    
    confirmDiv <- UI.div # set UI.style [("display", "none"), ("border", "1px solid #ddd"), ("padding", "20px"), ("background-color", "#fff3e0"), ("border-radius", "4px"), ("margin-bottom", "20px")]
    element confirmDiv #+ [element confirmHeader, element confirmMessage, element confirmButtonDiv]
    
    -- TVar to track selected contact and form mode
    selectedContactVar <- liftIO $ newTVarIO (Nothing :: Maybe ContactId)
    formModeVar <- liftIO $ newTVarIO AddMode
    deleteTargetVar <- liftIO $ newTVarIO (Nothing :: Maybe ContactId)

    
    -- Helper function to refresh the contact list
    let refreshContactList = do
            appState <- liftIO $ getAppState stateManager
            let term = searchTerm appState
            let allContacts = Map.elems (contacts appState)
            let filteredContacts = searchContacts term allContacts
            
            -- Clear existing rows
            element contactTableBody # set children []
            
            -- Show/hide empty message
            if null filteredContacts
                then element emptyMessage # set UI.style [("display", "block")]
                else element emptyMessage # set UI.style [("display", "none")]
            
            -- Add rows for each contact
            forM_ filteredContacts $ \contact -> do
                selectedId <- liftIO $ readTVarIO selectedContactVar
                let isSelected = selectedId == Just (contactId contact)
                row <- createContactRow contact isSelected selectedContactVar refreshContactList
                element contactTableBody #+ [element row]
    
    -- Helper function to clear form
    let clearForm = do
            element nameInput # set UI.value ""
            element phoneInput # set UI.value ""
            element emailInput # set UI.value ""
            element addressInput # set UI.value ""
            element errorDiv # set UI.style [("display", "none")]
    
    -- Helper function to show error
    let showError msg = do
            element errorDiv # set text msg
            element errorDiv # set UI.style [("display", "block")]
    
    -- Helper function to hide error
    let hideError = element errorDiv # set UI.style [("display", "none")]
    
    -- Helper function to save contacts to file
    let saveToFile = do
            appState <- liftIO $ getAppState stateManager
            let contactList = Map.elems (contacts appState)
            void $ liftIO $ saveContactsToFile contactsFilePath contactList
    
    -- Search functionality
    on UI.valueChange searchInput $ \searchText -> do
        liftIO $ updateAppState stateManager $ \state -> state { searchTerm = T.pack searchText }
        void refreshContactList
    
    -- Add button click handler
    on UI.click addButton $ \_ -> do
        liftIO $ atomically $ writeTVar formModeVar AddMode
        void clearForm
        void $ element formHeader # set text "Add Contact"
        void $ element formDiv # set UI.style [("display", "block")]
        void $ element confirmDiv # set UI.style [("display", "none")]
    
    -- Edit button click handler
    on UI.click editButton $ \_ -> do
        selectedId <- liftIO $ readTVarIO selectedContactVar
        case selectedId of
            Nothing -> return ()
            Just cid -> do
                appState <- liftIO $ getAppState stateManager
                case Map.lookup cid (contacts appState) of
                    Nothing -> return ()
                    Just contact -> do
                        liftIO $ atomically $ writeTVar formModeVar (EditMode cid)
                        void $ element nameInput # set UI.value (T.unpack $ contactName contact)
                        void $ element phoneInput # set UI.value (T.unpack $ fromMaybe "" $ contactPhone contact)
                        void $ element emailInput # set UI.value (T.unpack $ fromMaybe "" $ contactEmail contact)
                        void $ element addressInput # set UI.value (T.unpack $ fromMaybe "" $ contactAddress contact)
                        void hideError
                        void $ element formHeader # set text "Edit Contact"
                        void $ element formDiv # set UI.style [("display", "block")]
                        void $ element confirmDiv # set UI.style [("display", "none")]
    
    -- Delete button click handler
    on UI.click deleteButton $ \_ -> do
        selectedId <- liftIO $ readTVarIO selectedContactVar
        case selectedId of
            Nothing -> return ()
            Just cid -> do
                liftIO $ atomically $ writeTVar deleteTargetVar (Just cid)
                void $ element confirmDiv # set UI.style [("display", "block")]
                void $ element formDiv # set UI.style [("display", "none")]

    
    -- Save button click handler
    on UI.click saveButton $ \_ -> do
        nameVal <- T.pack <$> get UI.value nameInput
        phoneVal <- T.pack <$> get UI.value phoneInput
        emailVal <- T.pack <$> get UI.value emailInput
        addressVal <- T.pack <$> get UI.value addressInput
        
        formMode <- liftIO $ readTVarIO formModeVar
        appState <- liftIO $ getAppState stateManager
        
        let phoneM = if T.null (T.strip phoneVal) then Nothing else Just phoneVal
        let emailM = if T.null (T.strip emailVal) then Nothing else Just emailVal
        let addressM = if T.null (T.strip addressVal) then Nothing else Just addressVal
        
        let contactData = case formMode of
                AddMode -> Contact (nextId appState) nameVal phoneM emailM addressM
                EditMode cid -> Contact cid nameVal phoneM emailM addressM
        
        let result = case formMode of
                AddMode -> addContact contactData appState
                EditMode _ -> updateContact contactData appState
        
        case result of
            Left errors -> do
                let errorMsg = formatErrors errors
                void $ showError errorMsg
            Right newState -> do
                liftIO $ setAppState stateManager newState
                saveToFile
                void clearForm
                void $ element formDiv # set UI.style [("display", "none")]
                void refreshContactList
    
    -- Cancel button click handler
    on UI.click cancelButton $ \_ -> do
        void clearForm
        void $ element formDiv # set UI.style [("display", "none")]
    
    -- Confirm delete yes button
    on UI.click confirmYesButton $ \_ -> do
        targetId <- liftIO $ readTVarIO deleteTargetVar
        case targetId of
            Nothing -> return ()
            Just cid -> do
                appState <- liftIO $ getAppState stateManager
                let newState = deleteContact cid appState
                liftIO $ setAppState stateManager newState
                liftIO $ atomically $ writeTVar selectedContactVar Nothing
                liftIO $ atomically $ writeTVar deleteTargetVar Nothing
                saveToFile
                void $ element confirmDiv # set UI.style [("display", "none")]
                void refreshContactList
    
    -- Confirm delete no button
    on UI.click confirmNoButton $ \_ -> do
        liftIO $ atomically $ writeTVar deleteTargetVar Nothing
        void $ element confirmDiv # set UI.style [("display", "none")]
    
    -- Helper function to trigger add contact action (for keyboard shortcut)
    let triggerAddContact = do
            liftIO $ atomically $ writeTVar formModeVar AddMode
            void clearForm
            void $ element formHeader # set text "Add Contact"
            void $ element formDiv # set UI.style [("display", "block")]
            void $ element confirmDiv # set UI.style [("display", "none")]
    
    -- Helper function to trigger delete contact action (for keyboard shortcut)
    let triggerDeleteContact = do
            selectedId <- liftIO $ readTVarIO selectedContactVar
            case selectedId of
                Nothing -> return ()
                Just cid -> do
                    liftIO $ atomically $ writeTVar deleteTargetVar (Just cid)
                    void $ element confirmDiv # set UI.style [("display", "block")]
                    void $ element formDiv # set UI.style [("display", "none")]
    
    -- Helper function to trigger cancel operation (for keyboard shortcut)
    let triggerCancelOperation = do
            -- Cancel form if visible
            void clearForm
            void $ element formDiv # set UI.style [("display", "none")]
            -- Cancel delete confirmation if visible
            liftIO $ atomically $ writeTVar deleteTargetVar Nothing
            void $ element confirmDiv # set UI.style [("display", "none")]
    
    -- Helper function to focus search input (for keyboard shortcut)
    let triggerFocusSearch = do
            runFunction $ ffi "$(%1).focus()" searchInput
    
    -- Create hidden buttons for keyboard shortcut triggers
    -- These buttons are clicked programmatically from JavaScript
    shortcutNewBtn <- UI.button # set (UI.attr "id") "shortcut-new"
                                # set UI.style [("display", "none")]
    shortcutDeleteBtn <- UI.button # set (UI.attr "id") "shortcut-delete"
                                   # set UI.style [("display", "none")]
    shortcutCancelBtn <- UI.button # set (UI.attr "id") "shortcut-cancel"
                                   # set UI.style [("display", "none")]
    shortcutSearchBtn <- UI.button # set (UI.attr "id") "shortcut-search"
                                   # set UI.style [("display", "none")]
    
    -- Wire up hidden buttons to their actions
    on UI.click shortcutNewBtn $ \_ -> triggerAddContact
    on UI.click shortcutDeleteBtn $ \_ -> triggerDeleteContact
    on UI.click shortcutCancelBtn $ \_ -> triggerCancelOperation
    on UI.click shortcutSearchBtn $ \_ -> triggerFocusSearch
    
    -- Set up keyboard shortcuts using JavaScript FFI
    -- Register global keydown event listener on the document
    runFunction $ ffi 
        "document.addEventListener('keydown', function(e) { \
        \  var keyCode = e.keyCode || e.which; \
        \  var ctrlKey = e.ctrlKey || e.metaKey; \
        \  var activeTag = document.activeElement ? document.activeElement.tagName.toLowerCase() : ''; \
        \  var isInputFocused = (activeTag === 'input' || activeTag === 'textarea'); \
        \  if (ctrlKey && keyCode === 78) { \
        \    e.preventDefault(); \
        \    var btn = document.getElementById('shortcut-new'); \
        \    if (btn) btn.click(); \
        \  } else if (ctrlKey && keyCode === 70) { \
        \    e.preventDefault(); \
        \    var btn = document.getElementById('shortcut-search'); \
        \    if (btn) btn.click(); \
        \  } else if (keyCode === 46 && !isInputFocused) { \
        \    e.preventDefault(); \
        \    var btn = document.getElementById('shortcut-delete'); \
        \    if (btn) btn.click(); \
        \  } else if (keyCode === 27) { \
        \    e.preventDefault(); \
        \    var btn = document.getElementById('shortcut-cancel'); \
        \    if (btn) btn.click(); \
        \  } \
        \});"
    
    -- Assemble the layout
    element container #+ 
        [ element header
        , element searchDiv
        , element actionDiv
        , element formDiv
        , element confirmDiv
        , element listDiv
        -- Hidden buttons for keyboard shortcuts
        , element shortcutNewBtn
        , element shortcutDeleteBtn
        , element shortcutCancelBtn
        , element shortcutSearchBtn
        ]
    
    getBody window #+ [element container]
    
    -- Initial contact list render
    refreshContactList
    return ()

-- | Create a styled button
createButton :: String -> String -> UI Element
createButton label bgColor = 
    UI.button # set text label
             # set UI.style 
                 [ ("background-color", bgColor)
                 , ("color", "white")
                 , ("padding", "10px 20px")
                 , ("border", "none")
                 , ("border-radius", "4px")
                 , ("cursor", "pointer")
                 , ("margin-right", "10px")
                 ]

-- | Create a contact row with selection and delete functionality
createContactRow :: Contact -> Bool -> TVar (Maybe ContactId) -> UI () -> UI Element
createContactRow contact isSelected selectedVar refreshList = do
    let bgColor = if isSelected then "#e3f2fd" else "white"
    
    row <- UI.tr # set UI.style [("background-color", bgColor), ("cursor", "pointer")]
    
    -- Radio button for selection
    radioBtn <- UI.input # set UI.type_ "radio"
                        # set (UI.attr "name") "contact-select"
    when isSelected $ void $ element radioBtn # set (UI.attr "checked") "checked"
    
    selectCell <- UI.td # set UI.style [("border", "1px solid #ddd"), ("padding", "8px"), ("text-align", "center")]
    element selectCell #+ [element radioBtn]
    
    nameCell <- UI.td # set text (T.unpack $ contactName contact)
                     # set UI.style [("border", "1px solid #ddd"), ("padding", "8px")]
    phoneCell <- UI.td # set text (T.unpack $ fromMaybe "" $ contactPhone contact)
                      # set UI.style [("border", "1px solid #ddd"), ("padding", "8px")]
    emailCell <- UI.td # set text (T.unpack $ fromMaybe "" $ contactEmail contact)
                      # set UI.style [("border", "1px solid #ddd"), ("padding", "8px")]
    addressCell <- UI.td # set text (T.unpack $ fromMaybe "" $ contactAddress contact)
                        # set UI.style [("border", "1px solid #ddd"), ("padding", "8px")]
    
    -- Delete button in row
    deleteBtn <- UI.button # set text "×"
                          # set UI.style 
                              [ ("background-color", "#f44336")
                              , ("color", "white")
                              , ("border", "none")
                              , ("border-radius", "4px")
                              , ("cursor", "pointer")
                              , ("padding", "5px 10px")
                              , ("font-size", "14px")
                              ]
    actionsCell <- UI.td # set UI.style [("border", "1px solid #ddd"), ("padding", "8px"), ("text-align", "center")]
    element actionsCell #+ [element deleteBtn]
    
    element row #+ [element selectCell, element nameCell, element phoneCell, element emailCell, element addressCell, element actionsCell]
    
    -- Row click handler for selection
    on UI.click row $ \_ -> do
        liftIO $ atomically $ writeTVar selectedVar (Just $ contactId contact)
        refreshList
    
    -- Radio button click handler
    on UI.click radioBtn $ \_ -> do
        liftIO $ atomically $ writeTVar selectedVar (Just $ contactId contact)
        refreshList
    
    return row

-- | Format validation errors for display
formatErrors :: [ValidationError] -> String
formatErrors errors = unlines $ map formatError errors
  where
    formatError EmptyName = "Name is required and cannot be empty."
    formatError InvalidEmail = "Invalid email format."
    formatError InvalidPhone = "Invalid phone number format. Use only digits, spaces, hyphens, and parentheses."
