module Application.Services.ChatApplicationService
    ( OpenAIChatApplicationService
    , createOpenAIChatApplicationService
    ) where

import           Application.Interfaces.ChatApplicationService (ChatApplicationService (..))
import           Application.Services.ChatService              (OpenAIChatService)

import           Control.Monad                                 (when)

import           Data.Text                                     (pack)
import qualified Data.Text.IO                                  as TIO
import           Data.UUID.V4                                  (nextRandom)

import           Domain.Entities.Message                       (Message (..),
                                                                Role (..))
import           Domain.Interfaces.ChatService                 (ChatService (..))

import           Flow                                          ((<|))

import           System.IO                                     (hFlush, stdout)

-- | OpenAI Chat Application Service implementation
data OpenAIChatApplicationService = OpenAIChatApplicationService { ocasChatService :: OpenAIChatService
                                                                   -- ^ The underlying chat service
                                                                 }

-- | Create a new OpenAI Chat Application Service
createOpenAIChatApplicationService :: OpenAIChatService -> OpenAIChatApplicationService
createOpenAIChatApplicationService = OpenAIChatApplicationService

instance ChatApplicationService OpenAIChatApplicationService where
    handleChatRequest service inputMessage maybeSessionId = do
        -- Generate a new session ID if one wasn't provided
        sessionUUID <- maybe nextRandom return maybeSessionId

        -- Create a message from the input and run the chat
        let userMessage = Message User inputMessage
            chatService = ocasChatService service
        messages <- runChat chatService [userMessage]

        -- Extract the assistant's response
        let response = case messages of
                [_, Message _ content] -> content
                _                      -> pack "Error: Unexpected response format"

        -- Return the response and session ID
        return (response, sessionUUID)

    runInteractiveChat service = chatLoop (ocasChatService service) []
      where
        chatLoop chatService messages = do
            -- Prompt for user input
            TIO.putStr (pack "\nYou: ")
            hFlush stdout
            userInput <- TIO.getLine

            -- Check if user wants to exit
            let exitCommands = [pack ":q", pack "quit", pack "exit"]
            when (userInput `notElem` exitCommands) <| do
                -- Add user message to history and get AI response
                let newMessages = messages <> [Message User userInput]
                TIO.putStr (pack "Assistant: ")
                hFlush stdout
                updatedMessages <- runChat chatService newMessages

                -- Continue the conversation
                chatLoop chatService updatedMessages
