module Domain.Message exposing (Message, Role(..), create, roleToString)

{-| 메시지 도메인 엔티티
-}


type Role
    = User
    | Assistant


type alias Message =
    { role : Role
    , content : String
    }


create : Role -> String -> Message
create role content =
    { role = role
    , content = content
    }


roleToString : Role -> String
roleToString role =
    case role of
        User ->
            "user"

        Assistant ->
            "assistant"
