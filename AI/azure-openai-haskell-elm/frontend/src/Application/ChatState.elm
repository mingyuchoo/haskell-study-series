module Application.ChatState exposing
    ( ChatState
    , init
    , addMessage
    , clearMessages
    , setLoading
    , setError
    , clearError
    , updateInput
    , getMessages
    , getInput
    , isLoading
    , getError
    )

{-| 채팅 상태 관리
-}

import Domain.Message exposing (Message)


type alias ChatState =
    { messages : List Message
    , input : String
    , isLoading : Bool
    , error : Maybe String
    }


init : ChatState
init =
    { messages = []
    , input = ""
    , isLoading = False
    , error = Nothing
    }


addMessage : Message -> ChatState -> ChatState
addMessage message state =
    { state | messages = state.messages ++ [ message ] }


clearMessages : ChatState -> ChatState
clearMessages state =
    { state | messages = [], error = Nothing }


setLoading : Bool -> ChatState -> ChatState
setLoading loading state =
    { state | isLoading = loading }


setError : String -> ChatState -> ChatState
setError err state =
    { state | error = Just err, isLoading = False }


clearError : ChatState -> ChatState
clearError state =
    { state | error = Nothing }


updateInput : String -> ChatState -> ChatState
updateInput newInput state =
    { state | input = newInput }


getMessages : ChatState -> List Message
getMessages state =
    state.messages


getInput : ChatState -> String
getInput state =
    state.input


isLoading : ChatState -> Bool
isLoading state =
    state.isLoading


getError : ChatState -> Maybe String
getError state =
    state.error
