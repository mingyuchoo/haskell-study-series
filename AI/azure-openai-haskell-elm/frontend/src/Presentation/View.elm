module Presentation.View exposing (view)

{-| 메인 뷰 조합
-}

import Application.ChatState as ChatState exposing (ChatState)
import Html exposing (..)
import Html.Attributes exposing (..)
import Presentation.Components.Header as Header
import Presentation.Components.InputArea as InputArea
import Presentation.Components.MessageList as MessageList


type alias ViewConfig msg =
    { onUpdateInput : String -> msg
    , onSendMessage : msg
    , onClearChat : msg
    , onKeyDown : Int -> msg
    }


view : ViewConfig msg -> ChatState -> Html msg
view config state =
    div [ class "chat-container" ]
        [ Header.view
            (not (List.isEmpty (ChatState.getMessages state)))
            config.onClearChat
        , MessageList.view
            (ChatState.getMessages state)
            (ChatState.isLoading state)
            (ChatState.getError state)
        , InputArea.view
            (ChatState.getInput state)
            (ChatState.isLoading state)
            config.onUpdateInput
            config.onKeyDown
            config.onSendMessage
        ]
