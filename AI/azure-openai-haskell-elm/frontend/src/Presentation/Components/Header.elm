module Presentation.Components.Header exposing (view)

{-| 헤더 컴포넌트
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


view : Bool -> msg -> Html msg
view hasMessages clearMsg =
    header [ class "chat-header" ]
        [ h1 [] [ text "OpenAI Chat Assistant" ]
        , if hasMessages then
            button [ class "clear-button", onClick clearMsg ]
                [ text "New Chat" ]

          else
            text ""
        ]
