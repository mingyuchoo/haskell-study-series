module Presentation.Components.InputArea exposing (view)

{-| 입력 영역 컴포넌트
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode


view : String -> Bool -> (String -> msg) -> (Int -> msg) -> msg -> Html msg
view input loading onInputMsg onKeyDownMsg onSendMsg =
    div [ class "input-container" ]
        [ textarea
            [ value input
            , onInput onInputMsg
            , onKeyDown onKeyDownMsg
            , placeholder "Type your message... (Press Enter to send)"
            , disabled loading
            , rows 3
            ]
            []
        , button
            [ onClick onSendMsg
            , disabled (String.trim input == "" || loading)
            , class "send-button"
            ]
            [ text
                (if loading then
                    "⏳ Send"

                 else
                    "📤 Send"
                )
            ]
        ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Decode.map tagger keyCode)


keyCode : Decode.Decoder Int
keyCode =
    Decode.field "keyCode" Decode.int
