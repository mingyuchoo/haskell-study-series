module Tests.UnitTests
    ( unitTests
    ) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Models.Contact (Contact(..), ContactId(..))
import Models.AppState (AppState(..))
import Services.ValidationService (validateEmail, validatePhone, validateContactData, ValidationError(..))
import Services.SearchService (searchContacts)
import Services.ContactService (addContact, updateContact, deleteContact, generateNextId)
import Services.ContactRepository (loadContactsFromFile, saveContactsToFile)
import System.Directory (removeFile, doesFileExist)
import Control.Exception (try, IOException)

-- | Main unit test suite
unitTests :: Spec
unitTests = do
  describe "ValidationService" $ do
    validationTests
  
  describe "SearchService" $ do
    searchTests
    
  describe "ContactService" $ do
    contactServiceTests
    
  describe "ContactRepository" $ do
    repositoryTests

-- | Validation service tests
validationTests :: Spec
validationTests = do
  describe "validateEmail" $ do
    it "accepts valid email addresses" $ do
      validateEmail (Just "test@example.com") `shouldBe` True
      validateEmail (Just "user.name+tag@domain.co.uk") `shouldBe` True
      
    it "rejects invalid email addresses" $ do
      validateEmail (Just "invalid-email") `shouldBe` False
      validateEmail (Just "@domain.com") `shouldBe` False
      validateEmail (Just "user@") `shouldBe` False
      
    it "accepts Nothing (optional field)" $ do
      validateEmail Nothing `shouldBe` True
      
  describe "validatePhone" $ do
    it "accepts valid phone numbers" $ do
      validatePhone (Just "010-1234-5678") `shouldBe` True
      validatePhone (Just "(02) 123-4567") `shouldBe` True
      validatePhone (Just "123 456 7890") `shouldBe` True
      
    it "rejects invalid phone numbers" $ do
      validatePhone (Just "abc-def-ghij") `shouldBe` False
      validatePhone (Just "123-456-789a") `shouldBe` False
      
    it "accepts Nothing (optional field)" $ do
      validatePhone Nothing `shouldBe` True
      
  describe "validateContactData" $ do
    it "accepts valid contact" $ do
      let contact = Contact (ContactId 1) "John Doe" (Just "010-1234-5678") (Just "john@example.com") Nothing
      validateContactData contact `shouldBe` Right contact
      
    it "rejects contact with empty name" $ do
      let contact = Contact (ContactId 1) "" (Just "010-1234-5678") (Just "john@example.com") Nothing
      validateContactData contact `shouldBe` Left [EmptyName]
      
    it "rejects contact with invalid email" $ do
      let contact = Contact (ContactId 1) "John Doe" (Just "010-1234-5678") (Just "invalid-email") Nothing
      validateContactData contact `shouldBe` Left [InvalidEmail]

-- | Search service tests  
searchTests :: Spec
searchTests = do
  let contacts = [ Contact (ContactId 1) "John Doe" (Just "010-1234-5678") (Just "john@example.com") Nothing
                 , Contact (ContactId 2) "Jane Smith" (Just "010-9876-5432") (Just "jane@example.com") Nothing
                 , Contact (ContactId 3) "Bob Johnson" (Just "010-5555-1234") (Just "bob@test.com") Nothing
                 ]
                 
  describe "searchContacts" $ do
    it "returns all contacts for empty search" $ do
      searchContacts "" contacts `shouldBe` contacts
      searchContacts "   " contacts `shouldBe` contacts
      
    it "filters by name (case insensitive)" $ do
      let result = searchContacts "john" contacts
      length result `shouldBe` 2
      map contactName result `shouldContain` ["John Doe", "Bob Johnson"]
      
    it "filters by email" $ do
      let result = searchContacts "test.com" contacts
      length result `shouldBe` 1
      contactName (head result) `shouldBe` "Bob Johnson"
      
    it "filters by phone" $ do
      let result = searchContacts "9876" contacts
      length result `shouldBe` 1
      contactName (head result) `shouldBe` "Jane Smith"
      
    it "returns empty list for no matches" $ do
      searchContacts "nonexistent" contacts `shouldBe` []

-- | Contact service tests
contactServiceTests :: Spec
contactServiceTests = do
  let emptyState = AppState Map.empty (ContactId 1) ""
      sampleContact = Contact (ContactId 0) "Test User" (Just "010-1234-5678") (Just "test@example.com") Nothing
      
  describe "addContact" $ do
    it "adds valid contact to empty state" $ do
      case addContact sampleContact emptyState of
        Right newState -> do
          Map.size (contacts newState) `shouldBe` 1
          nextId newState `shouldBe` ContactId 2
        Left _ -> expectationFailure "Should have succeeded"
        
    it "rejects invalid contact" $ do
      let invalidContact = Contact (ContactId 0) "" Nothing Nothing Nothing
      case addContact invalidContact emptyState of
        Left errors -> errors `shouldContain` [EmptyName]
        Right _ -> expectationFailure "Should have failed"
        
  describe "updateContact" $ do
    it "updates existing contact" $ do
      -- First add a contact
      case addContact sampleContact emptyState of
        Right stateWithContact -> do
          let updatedContact = Contact (ContactId 1) "Updated Name" (Just "010-9999-9999") (Just "updated@example.com") Nothing
          case updateContact updatedContact stateWithContact of
            Right finalState -> do
              Map.size (contacts finalState) `shouldBe` 1
              let maybeContact = Map.lookup (ContactId 1) (contacts finalState)
              case maybeContact of
                Just contact -> contactName contact `shouldBe` "Updated Name"
                Nothing -> expectationFailure "Contact should exist"
            Left _ -> expectationFailure "Update should have succeeded"
        Left _ -> expectationFailure "Initial add should have succeeded"
        
  describe "deleteContact" $ do
    it "removes existing contact" $ do
      -- First add a contact
      case addContact sampleContact emptyState of
        Right stateWithContact -> do
          let finalState = deleteContact (ContactId 1) stateWithContact
          Map.size (contacts finalState) `shouldBe` 0
        Left _ -> expectationFailure "Initial add should have succeeded"
        
  describe "generateNextId" $ do
    it "increments ContactId correctly" $ do
      generateNextId (ContactId 1) `shouldBe` ContactId 2
      generateNextId (ContactId 99) `shouldBe` ContactId 100

-- | Repository tests
repositoryTests :: Spec
repositoryTests = do
  let testFile = "test_contacts.json"
      testContacts = [ Contact (ContactId 1) "Test User 1" (Just "010-1111-1111") (Just "test1@example.com") Nothing
                     , Contact (ContactId 2) "Test User 2" (Just "010-2222-2222") (Just "test2@example.com") Nothing
                     ]
  
  describe "file operations" $ do
    it "saves and loads contacts correctly" $ do
      -- Clean up any existing test file
      fileExists <- doesFileExist testFile
      if fileExists then removeFile testFile else return ()
      
      -- Save contacts
      saveResult <- saveContactsToFile testFile testContacts
      saveResult `shouldBe` Right ()
      
      -- Load contacts back
      loadResult <- loadContactsFromFile testFile
      case loadResult of
        Right loadedContacts -> do
          length loadedContacts `shouldBe` 2
          map contactName loadedContacts `shouldBe` ["Test User 1", "Test User 2"]
        Left err -> expectationFailure $ "Load failed: " ++ err
      
      -- Clean up
      removeFile testFile
      
    it "handles missing file gracefully" $ do
      let nonExistentFile = "nonexistent_file.json"
      result <- loadContactsFromFile nonExistentFile
      result `shouldBe` Right []
      
    it "handles corrupted file gracefully" $ do
      let corruptedFile = "corrupted_test.json"
      -- Create a corrupted JSON file
      writeFile corruptedFile "{ invalid json content"
      
      result <- loadContactsFromFile corruptedFile
      case result of
        Left _ -> return ()  -- Expected to fail
        Right _ -> expectationFailure "Should have failed on corrupted file"
      
      -- Clean up
      removeFile corruptedFile