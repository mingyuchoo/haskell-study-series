module Infrastructure.Http.Decoder exposing
    ( chatResponseDecoder
    , encodeMessage
    , encodeChatRequest
    )

{-| JSON 인코더/디코더
-}

import Domain.Message as Message exposing (Message)
import Json.Decode as Decode
import Json.Encode as Encode


chatResponseDecoder : Decode.Decoder String
chatResponseDecoder =
    Decode.field "response" Decode.string


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
        [ ( "msgRole", Encode.string (Message.roleToString message.role) )
        , ( "msgContent", Encode.string message.content )
        ]
