module Infrastructure.TaskApi exposing (perform)

import Application.TaskBoard exposing (ApiError(..), Effect(..), Msg(..), Profile, Session)
import Domain.Task as Task exposing (Status, Task, TaskInput)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        LoadTasks ->
            Http.get
                { url = "/api/task"
                , expect = Http.expectJson (GotTasks << Result.mapError toApiError) (Decode.list taskDecoder)
                }

        CreateTask input ->
            Http.post
                { url = "/api/task"
                , body = Http.jsonBody (taskInputEncoder input)
                , expect = expectTask Saved
                }

        UpdateTask taskId input ->
            Http.request
                { method = "PUT"
                , headers = []
                , url = "/api/task/" ++ String.fromInt taskId
                , body = Http.jsonBody (taskInputEncoder input)
                , expect = expectTask Saved
                , timeout = Nothing
                , tracker = Nothing
                }

        MoveTask taskId input ->
            Http.request
                { method = "PUT"
                , headers = []
                , url = "/api/task/" ++ String.fromInt taskId
                , body = Http.jsonBody (taskInputEncoder input)
                , expect = expectTask MoveSaved
                , timeout = Nothing
                , tracker = Nothing
                }

        DeleteTask taskId ->
            Http.request
                { method = "DELETE"
                , headers = []
                , url = "/api/task/" ++ String.fromInt taskId
                , body = Http.emptyBody
                , expect = Http.expectWhatever (Deleted << Result.mapError toApiError)
                , timeout = Nothing
                , tracker = Nothing
                }

        SubmitTaskResult taskId owner submittedResult ->
            Http.post
                { url = "/api/task/" ++ String.fromInt taskId ++ "/submit"
                , body = Http.jsonBody (submissionEncoder owner submittedResult)
                , expect = expectTask WorkflowSaved
                }

        ApproveTaskResult taskId owner comment ->
            Http.post
                { url = "/api/task/" ++ String.fromInt taskId ++ "/approve"
                , body = Http.jsonBody (reviewEncoder owner comment)
                , expect = expectTask WorkflowSaved
                }

        RequestTaskRevision taskId owner comment ->
            Http.post
                { url = "/api/task/" ++ String.fromInt taskId ++ "/revision"
                , body = Http.jsonBody (reviewEncoder owner comment)
                , expect = expectTask WorkflowSaved
                }

        Register email displayName password ->
            Http.post
                { url = "/api/auth/signup"
                , body = Http.jsonBody (signUpEncoder email displayName password)
                , expect = expectSession Authenticated
                }

        Login email password ->
            Http.post
                { url = "/api/auth/login"
                , body = Http.jsonBody (loginEncoder email password)
                , expect = expectSession Authenticated
                }

        UpdateProfile token displayName ->
            Http.request
                { method = "PUT"
                , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
                , url = "/api/auth/me"
                , body = Http.jsonBody (Encode.object [ ( "displayName", Encode.string displayName ) ])
                , expect = expectProfile ProfileSaved
                , timeout = Nothing
                , tracker = Nothing
                }

        Logout token ->
            Http.request
                { method = "POST"
                , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
                , url = "/api/auth/logout"
                , body = Http.emptyBody
                , expect = Http.expectWhatever (LoggedOut << Result.mapError toApiError)
                , timeout = Nothing
                , tracker = Nothing
                }

        ClearNoticeAfter _ ->
            Cmd.none


toApiError : Http.Error -> ApiError
toApiError _ =
    RequestFailed


{-| 업무 응답을 기대하되, 4xx/5xx 응답의 `error` 필드는 사용자에게 보여 줄 메시지로 보존한다.
-}
expectTask : (Result ApiError Task -> Msg) -> Http.Expect Msg
expectTask toMsg =
    Http.expectStringResponse toMsg
        (\response ->
            case response of
                Http.GoodStatus_ _ body ->
                    Decode.decodeString taskDecoder body
                        |> Result.mapError (\_ -> RequestFailed)

                Http.BadStatus_ _ body ->
                    Decode.decodeString (Decode.field "error" Decode.string) body
                        |> Result.map Rejected
                        |> Result.withDefault RequestFailed
                        |> Err

                _ ->
                    Err RequestFailed
        )


expectSession : (Result ApiError Session -> Msg) -> Http.Expect Msg
expectSession toMsg =
    Http.expectStringResponse toMsg (decodeResponse sessionDecoder)


expectProfile : (Result ApiError Profile -> Msg) -> Http.Expect Msg
expectProfile toMsg =
    Http.expectStringResponse toMsg (decodeResponse profileDecoder)


decodeResponse : Decoder value -> Http.Response String -> Result ApiError value
decodeResponse decoder response =
    case response of
        Http.GoodStatus_ _ body ->
            Decode.decodeString decoder body
                |> Result.mapError (\_ -> RequestFailed)

        Http.BadStatus_ _ body ->
            Decode.decodeString (Decode.field "error" Decode.string) body
                |> Result.map Rejected
                |> Result.withDefault RequestFailed
                |> Err

        _ ->
            Err RequestFailed


taskDecoder : Decoder Task
taskDecoder =
    Decode.map2
        (\base ( expected, submitted, review ) ->
            Task base.identifier base.title base.description base.status base.urgency base.importance base.taskOwner base.outcomeOwner expected submitted review
        )
        (Decode.map8
            (\identifier title description status urgency importance taskOwner outcomeOwner ->
                { identifier = identifier, title = title, description = description, status = status, urgency = urgency, importance = importance, taskOwner = taskOwner, outcomeOwner = outcomeOwner }
            )
            (Decode.field "taskId" Decode.int)
            (Decode.field "title" Decode.string)
            (Decode.field "description" Decode.string)
            (Decode.field "status" statusDecoder)
            (Decode.field "urgency" urgencyDecoder)
            (Decode.field "importance" importanceDecoder)
            (Decode.field "taskOwner" Decode.string)
            (Decode.field "outcomeOwner" Decode.string)
        )
        (Decode.map3 (\expected submitted review -> ( expected, submitted, review )) (Decode.field "expectedResult" Decode.string) (Decode.field "submittedResult" (Decode.nullable Decode.string)) (Decode.field "reviewComment" (Decode.nullable Decode.string)))


profileDecoder : Decoder Profile
profileDecoder =
    Decode.map3 Profile
        (Decode.field "id" Decode.int)
        (Decode.field "email" Decode.string)
        (Decode.field "displayName" Decode.string)


sessionDecoder : Decoder Session
sessionDecoder =
    Decode.map2 Session
        (Decode.field "token" Decode.string)
        (Decode.field "user" profileDecoder)


statusDecoder : Decoder Status
statusDecoder =
    Decode.string
        |> Decode.andThen
            (\rawStatus ->
                case Task.statusFromString rawStatus of
                    Just taskStatus ->
                        Decode.succeed taskStatus

                    Nothing ->
                        Decode.fail "Unknown task status"
            )


urgencyDecoder : Decoder Task.Urgency
urgencyDecoder =
    Decode.string
        |> Decode.andThen
            (\rawUrgency ->
                case Task.urgencyFromString rawUrgency of
                    Just taskUrgency ->
                        Decode.succeed taskUrgency

                    Nothing ->
                        Decode.fail "Unknown task urgency"
            )


importanceDecoder : Decoder Task.Importance
importanceDecoder =
    Decode.string
        |> Decode.andThen
            (\rawImportance ->
                case Task.importanceFromString rawImportance of
                    Just taskImportance ->
                        Decode.succeed taskImportance

                    Nothing ->
                        Decode.fail "Unknown task importance"
            )


taskInputEncoder : TaskInput -> Encode.Value
taskInputEncoder input =
    Encode.object
        [ ( "title", Encode.string input.title )
        , ( "description", Encode.string input.description )
        , ( "status", Encode.string (Task.statusString input.status) )
        , ( "urgency", Encode.string (Task.urgencyString input.urgency) )
        , ( "importance", Encode.string (Task.importanceString input.importance) )
        , ( "taskOwner", Encode.string input.taskOwner )
        , ( "outcomeOwner", Encode.string input.outcomeOwner )
        , ( "expectedResult", Encode.string input.expectedResult )
        ]


submissionEncoder : String -> String -> Encode.Value
submissionEncoder owner submittedResult =
    Encode.object
        [ ( "taskOwner", Encode.string owner )
        , ( "submittedResult", Encode.string submittedResult )
        ]


reviewEncoder : String -> Maybe String -> Encode.Value
reviewEncoder owner comment =
    Encode.object
        (( "outcomeOwner", Encode.string owner )
            :: (case comment of
                    Just text ->
                        [ ( "reviewComment", Encode.string text ) ]

                    Nothing ->
                        []
               )
        )


signUpEncoder : String -> String -> String -> Encode.Value
signUpEncoder email displayName password =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "displayName", Encode.string displayName )
        , ( "password", Encode.string password )
        ]


loginEncoder : String -> String -> Encode.Value
loginEncoder email password =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "password", Encode.string password )
        ]
