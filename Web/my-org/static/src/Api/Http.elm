module Api.Http exposing (agents, discovery, errorText, organizations, send, workspace)

import Api.Agents
import Api.Decode exposing (..)
import Api.Discovery
import Api.Path exposing (orgPath)
import Domain exposing (Summary, Workspace)
import Domain.Agent
import Domain.Discovery exposing (Snapshot)
import Http
import Json.Decode as D
import Json.Encode as E


errorText : Http.Error -> String
errorText err =
    case err of
        Http.BadUrl _ ->
            "요청 주소를 확인할 수 없습니다."

        Http.Timeout ->
            "서버 응답 시간이 초과되었습니다. 입력 내용은 보존됩니다."

        Http.NetworkError ->
            "서버에 연결할 수 없습니다. 연결을 확인하고 다시 시도하세요."

        Http.BadStatus code ->
            "서버 조회 실패 (" ++ String.fromInt code ++ "). 새로고침해 주세요."

        Http.BadBody _ ->
            "서버 응답 형식이 예상과 다릅니다. 입력 내용은 보존됩니다."


organizations : (Result Http.Error (List Summary) -> msg) -> Cmd msg
organizations onResult =
    Http.get { url = "/api/organizations", expect = Http.expectJson onResult (D.list summaryDecoder) }


workspace : String -> (Result Http.Error Workspace -> msg) -> Cmd msg
workspace org onResult =
    Http.get { url = orgPath org "dashboard", expect = Http.expectJson onResult workspaceDecoder }


send : (Result String () -> msg) -> String -> String -> E.Value -> Cmd msg
send onResult method path body =
    Http.request
        { method = method
        , headers = []
        , url = path
        , body = Http.jsonBody body
        , timeout = Just 30000
        , tracker = Nothing
        , expect =
            Http.expectStringResponse onResult
                (\response ->
                    case response of
                        Http.BadUrl_ _ ->
                            Err "잘못된 요청 주소입니다."

                        Http.Timeout_ ->
                            Err "응답 시간이 초과되었습니다. 서버에서 이미 저장됐을 수 있으므로 최신 기록을 확인하세요."

                        Http.NetworkError_ ->
                            Err "연결이 끊겼습니다. 서버에서 이미 저장됐을 수 있으므로 최신 기록을 확인하세요."

                        Http.BadStatus_ metadata content ->
                            Err ((D.decodeString (D.field "error" D.string) content |> Result.withDefault "요청을 처리할 수 없습니다.") ++ " (" ++ String.fromInt metadata.statusCode ++ ")")

                        Http.GoodStatus_ _ _ ->
                            Ok ()
                )
        }


discovery : String -> (Result Http.Error Snapshot -> msg) -> Cmd msg
discovery org onResult =
    Http.get { url = orgPath org "discovery", expect = Http.expectJson onResult Api.Discovery.decoder }


agents : String -> (Result Http.Error Domain.Agent.Snapshot -> msg) -> Cmd msg
agents org onResult =
    Http.get { url = orgPath org "agents", expect = Http.expectJson onResult Api.Agents.decoder }
