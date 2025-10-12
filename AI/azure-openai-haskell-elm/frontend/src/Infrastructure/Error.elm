module Infrastructure.Error exposing (httpErrorToString)

{-| 에러 처리 유틸리티
-}

import Http


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
