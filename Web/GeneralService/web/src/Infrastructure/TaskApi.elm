module Infrastructure.TaskApi exposing (perform)

import Application.TaskBoard exposing (ApiError(..), Effect(..), Msg(..))
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
                , expect = Http.expectJson (Saved << Result.mapError toApiError) taskDecoder
                }

        UpdateTask taskId input ->
            Http.request
                { method = "PUT"
                , headers = []
                , url = "/api/task/" ++ String.fromInt taskId
                , body = Http.jsonBody (taskInputEncoder input)
                , expect = Http.expectJson (Saved << Result.mapError toApiError) taskDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

        MoveTask taskId input ->
            Http.request
                { method = "PUT"
                , headers = []
                , url = "/api/task/" ++ String.fromInt taskId
                , body = Http.jsonBody (taskInputEncoder input)
                , expect = Http.expectJson (MoveSaved << Result.mapError toApiError) taskDecoder
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

        ClearNoticeAfter _ ->
            Cmd.none


toApiError : Http.Error -> ApiError
toApiError _ =
    RequestFailed


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
