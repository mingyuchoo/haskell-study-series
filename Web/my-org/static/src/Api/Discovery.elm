module Api.Discovery exposing (decoder, encode)

import Api.Decode exposing (field)
import Domain.Discovery exposing (..)
import Json.Decode as D
import Json.Encode as E


decoder : D.Decoder Snapshot
decoder =
    D.map2 Snapshot (D.field "version" D.int) (D.field "discovery" documentDecoder)


documentDecoder =
    D.succeed Document |> field "scope" D.string |> field "asOf" D.string |> field "observations" (D.list observationDecoder) |> field "workflows" (D.list workflowDecoder) |> field "review" (D.map2 Review (D.field "status" reviewStatusDecoder) (D.field "note" D.string))


statusDecoder =
    D.string
        |> D.andThen
            (\value ->
                if List.member value [ "confirmed", "unknown", "proposed" ] then
                    D.succeed value

                else
                    D.fail "현황 확인 상태를 해석할 수 없습니다."
            )


observationDecoder =
    D.succeed Observation |> field "id" D.string |> field "subject" D.string |> field "detail" D.string |> field "status" statusDecoder |> field "evidence" D.string


workflowDecoder =
    D.succeed Workflow |> field "id" D.string |> field "name" D.string |> field "role" D.string |> field "trigger" D.string |> field "inputs" D.string |> field "tools" D.string |> field "outputs" D.string |> field "handoff" D.string |> field "approval" D.string |> field "status" statusDecoder |> field "evidence" D.string


strings pairs =
    E.object (List.map (Tuple.mapSecond E.string) pairs)


encode : Document -> E.Value
encode doc =
    E.object
        [ ( "scope", E.string doc.scope )
        , ( "asOf", E.string doc.asOf )
        , ( "observations", E.list (\o -> strings [ ( "id", o.id ), ( "subject", o.subject ), ( "detail", o.detail ), ( "status", o.status ), ( "evidence", o.evidence ) ]) doc.observations )
        , ( "workflows", E.list (\w -> strings [ ( "id", w.id ), ( "name", w.name ), ( "role", w.role ), ( "trigger", w.trigger ), ( "inputs", w.inputs ), ( "tools", w.tools ), ( "outputs", w.outputs ), ( "handoff", w.handoff ), ( "approval", w.approval ), ( "status", w.status ), ( "evidence", w.evidence ) ]) doc.workflows )
        , ( "review", strings [ ( "status", doc.review.status ), ( "note", doc.review.note ) ] )
        ]


reviewStatusDecoder =
    D.string
        |> D.andThen
            (\value ->
                if List.member value [ "pending", "reviewed" ] then
                    D.succeed value

                else
                    D.fail "검토 상태를 해석할 수 없습니다."
            )
