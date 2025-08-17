module Application.Interfaces.ChatApplicationService
    ( ChatApplicationService (..)
    ) where

import           Data.Text (Text)
import           Data.UUID (UUID)

-- | Interface for chat application services that manage user interactions
class ChatApplicationService s where
    -- | Handle a single chat request and return the response with session ID
    handleChatRequest :: s               -- ^ Service instance
                      -> Text            -- ^ User input message
                      -> Maybe UUID      -- ^ Optional session ID (creates new one if Nothing)
                      -> IO (Text, UUID) -- ^ (Response text, session ID)

    -- | Run an interactive chat session in the terminal
    runInteractiveChat :: s -> IO ()
