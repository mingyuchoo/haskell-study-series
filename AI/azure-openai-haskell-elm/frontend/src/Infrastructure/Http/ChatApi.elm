module Infrastructure.Http.ChatApi exposing (chatService)

{-| HTTP API 클라이언트 구현
-}

import Domain.ChatService exposing (ChatService)
import Domain.Message exposing (Message)
import Http
import Infrastructure.Error exposing (httpErrorToString)
import Infrastructure.Http.Decoder as Decoder


chatService : (Result String String -> msg) -> ChatService msg
chatService toMsg =
    { sendMessage = sendChatRequest toMsg
    }


sendChatRequest : (Result String String -> msg) -> String -> List Message -> Cmd msg
sendChatRequest toMsg apiUrl messages =
    Http.post
        { url = apiUrl ++ "/api/chat"
        , body = Http.jsonBody (Decoder.encodeChatRequest messages)
        , expect = Http.expectJson (toMsg << Result.mapError httpErrorToString) Decoder.chatResponseDecoder
        }
