module Presentation.Components.MessageList exposing (view)

{-| 메시지 리스트 컴포넌트
-}

import Domain.Message as Message exposing (Message)
import Html exposing (..)
import Html.Attributes exposing (..)


view : List Message -> Bool -> Maybe String -> Html msg
view messages loading error =
    div [ class "messages-container" ]
        (if List.isEmpty messages then
            [ viewWelcome ]

         else
            List.map viewMessage messages
                ++ (if loading then
                        [ viewLoadingMessage ]

                    else
                        []
                   )
                ++ (case error of
                        Just err ->
                            [ viewError err ]

                        Nothing ->
                            []
                   )
        )


viewWelcome : Html msg
viewWelcome =
    div [ class "welcome-message" ]
        [ h2 [] [ text "Welcome to OpenAI Chat" ]
        , p [] [ text "Start a conversation by typing a message below" ]
        ]


viewMessage : Message -> Html msg
viewMessage message =
    let
        ( roleClass, roleLabel ) =
            case message.role of
                Message.User ->
                    ( "user", "👤 You" )

                Message.Assistant ->
                    ( "assistant", "🤖 Assistant" )
    in
    div [ class ("message " ++ roleClass) ]
        [ div [ class "message-role" ] [ text roleLabel ]
        , div [ class "message-content" ] [ text message.content ]
        ]


viewLoadingMessage : Html msg
viewLoadingMessage =
    div [ class "message assistant" ]
        [ div [ class "message-role" ] [ text "🤖 Assistant" ]
        , div [ class "message-content loading" ] [ text "Thinking..." ]
        ]


viewError : String -> Html msg
viewError err =
    div [ class "error-message" ]
        [ text ("⚠️ Error: " ++ err) ]
