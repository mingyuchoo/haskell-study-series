module Models.AppState
    ( AppState(..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Models.Contact (Contact, ContactId)

-- | Application state
data AppState = AppState
    { contacts   :: Map ContactId Contact
    , nextId     :: ContactId
    , searchTerm :: Text
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)