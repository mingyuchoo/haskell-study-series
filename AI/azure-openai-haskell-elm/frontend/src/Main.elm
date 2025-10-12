module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


-- MAIN

main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- MODEL

type alias Flags =
    { apiUrl : String
    }

type alias Message =
    { role : String
    , content : String
    }

type alias Model =
    { messages : List Message
    , input : String
    , isLoading : Bool
    , error : Maybe String
    , apiUrl : String
    }

init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { messages = []
      , input = ""
      , isLoading = False
      , error = Nothing
      , apiUrl = flags.apiUrl
      }
    , Cmd.none
    )


-- UPDATE

type Msg
    = UpdateInput String
    | SendMessage
    | GotResponse (Result Http.Error String)
    | ClearChat
    | KeyDown Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput newInput ->
            ( { model | input = newInput }, Cmd.none )

        SendMessage ->
            if String.trim model.input == "" || model.isLoading then
                ( model, Cmd.none )

            else
                let
                    userMessage =
                        { role = "user", content = String.trim model.input }

                    newMessages =
                        model.messages ++ [ userMessage ]
                in
                ( { model
                    | messages = newMessages
                    , input = ""
                    , isLoading = True
                    , error = Nothing
                  }
                , sendChatRequest model.apiUrl newMessages
                )

        GotResponse result ->
            case result of
                Ok response ->
                    let
                        assistantMessage =
                            { role = "assistant", content = response }
                    in
                    ( { model
                        | messages = model.messages ++ [ assistantMessage ]
                        , isLoading = False
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model
                        | isLoading = False
                        , error = Just (httpErrorToString error)
                      }
                    , Cmd.none
                    )

        ClearChat ->
            ( { model | messages = [], error = Nothing }, Cmd.none )

        KeyDown key ->
            if key == 13 then
                update SendMessage model

            else
                ( model, Cmd.none )


-- HTTP

sendChatRequest : String -> List Message -> Cmd Msg
sendChatRequest apiUrl messages =
    Http.post
        { url = apiUrl ++ "/api/chat"
        , body = Http.jsonBody (encodeChatRequest messages)
        , expect = Http.expectJson GotResponse chatResponseDecoder
        }

encodeChatRequest : List Message -> Encode.Value
encodeChatRequest messages =
    Encode.object
        [ ( "chatMessages"
          , Encode.list encodeMessage messages
          )
        ]

encodeMessage : Message -> Encode.Value
encodeMessage message =
    Encode.object
        [ ( "msgRole", Encode.string message.role )
        , ( "msgContent", Encode.string message.content )
        ]

chatResponseDecoder : Decode.Decoder String
chatResponseDecoder =
    Decode.field "response" Decode.string

httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "잘못된 URL: " ++ url

        Http.Timeout ->
            "요청 시간 초과"

        Http.NetworkError ->
            "네트워크 오류"

        Http.BadStatus status ->
            "서버 오류: " ++ String.fromInt status

        Http.BadBody body ->
            "응답 파싱 오류: " ++ body


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- VIEW

view : Model -> Html Msg
view model =
    div [ class "chat-container" ]
        [ viewHeader model
        , viewMessages model
        , viewInput model
        ]

viewHeader : Model -> Html Msg
viewHeader model =
    header [ class "chat-header" ]
        [ h1 [] [ text "OpenAI Chat Assistant" ]
        , if List.isEmpty model.messages then
            text ""

          else
            button [ class "clear-button", onClick ClearChat ]
                [ text "New Chat" ]
        ]

viewMessages : Model -> Html Msg
viewMessages model =
    div [ class "messages-container" ]
        (if List.isEmpty model.messages then
            [ viewWelcome ]

         else
            List.map viewMessage model.messages
                ++ (if model.isLoading then
                        [ viewLoadingMessage ]

                    else
                        []
                   )
                ++ (case model.error of
                        Just err ->
                            [ viewError err ]

                        Nothing ->
                            []
                   )
        )

viewWelcome : Html Msg
viewWelcome =
    div [ class "welcome-message" ]
        [ h2 [] [ text "Welcome to OpenAI Chat" ]
        , p [] [ text "Start a conversation by typing a message below" ]
        ]

viewMessage : Message -> Html Msg
viewMessage message =
    div [ class ("message " ++ message.role) ]
        [ div [ class "message-role" ]
            [ text
                (if message.role == "user" then
                    "👤 You"

                 else
                    "🤖 Assistant"
                )
            ]
        , div [ class "message-content" ] [ text message.content ]
        ]

viewLoadingMessage : Html Msg
viewLoadingMessage =
    div [ class "message assistant" ]
        [ div [ class "message-role" ] [ text "🤖 Assistant" ]
        , div [ class "message-content loading" ] [ text "Thinking..." ]
        ]

viewError : String -> Html Msg
viewError err =
    div [ class "error-message" ]
        [ text ("⚠️ Error: " ++ err) ]

viewInput : Model -> Html Msg
viewInput model =
    div [ class "input-container" ]
        [ textarea
            [ value model.input
            , onInput UpdateInput
            , onKeyDown KeyDown
            , placeholder "Type your message... (Press Enter to send)"
            , disabled model.isLoading
            , rows 3
            ]
            []
        , button
            [ onClick SendMessage
            , disabled (String.trim model.input == "" || model.isLoading)
            , class "send-button"
            ]
            [ text
                (if model.isLoading then
                    "⏳ Send"

                 else
                    "📤 Send"
                )
            ]
        ]


-- CUSTOM EVENT HANDLERS

onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Decode.map tagger keyCode)

keyCode : Decode.Decoder Int
keyCode =
    Decode.field "keyCode" Decode.int
