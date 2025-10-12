module Domain.ChatService exposing (ChatService, sendMessage)

{-| 채팅 서비스 인터페이스 (포트 정의)
-}

import Domain.Message exposing (Message)


type alias ChatService msg =
    { sendMessage : String -> List Message -> Cmd msg
    }


sendMessage : ChatService msg -> String -> List Message -> Cmd msg
sendMessage service apiUrl messages =
    service.sendMessage apiUrl messages
