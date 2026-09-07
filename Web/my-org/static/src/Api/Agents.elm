module Api.Agents exposing (decoder, encode, roleDecoder)

import Api.Decode exposing (andMap, diagnosticDecoder, field, optional)
import Domain.Agent exposing (..)
import Json.Decode as D
import Json.Encode as E


decoder : D.Decoder Snapshot
decoder =
    D.succeed Snapshot
        |> field "version" D.int
        |> field "agents" (D.list roleDecoder)
        |> field "drafts" (D.list roleDecoder)
        |> field "diagnostics" (D.list diagnosticDecoder)
        |> field "draftDiagnostics" (D.list diagnosticDecoder)


levelDecoder : D.Decoder String
levelDecoder =
    D.string
        |> D.andThen
            (\value ->
                if List.member value [ "L0", "L1", "L2", "L3" ] then
                    D.succeed value

                else
                    D.fail "권한 등급을 해석할 수 없습니다."
            )


approvalDecoder : D.Decoder Approval
approvalDecoder =
    D.oneOf [ D.field "person" D.string |> D.map Person, D.field "permission" D.string |> D.map Permission ]


roleDecoder : D.Decoder Role
roleDecoder =
    D.succeed Role
        |> field "id" D.string
        |> field "name" D.string
        |> andMap (optional "sourceWorkflow" D.string)
        |> field "task" D.string
        |> field "inputs" D.string
        |> field "outputs" D.string
        |> field "tools" (D.list D.string)
        |> field "permissionLevel" levelDecoder
        |> andMap (optional "approvalBy" approvalDecoder)
        |> field "handoffTo" (D.list D.string)
        |> field "status" D.string
        |> field "evidence" D.string


encodeRole : Role -> E.Value
encodeRole role =
    E.object
        ([ ( "id", E.string role.id )
         , ( "name", E.string role.name )
         , ( "task", E.string role.task )
         , ( "inputs", E.string role.inputs )
         , ( "outputs", E.string role.outputs )
         , ( "tools", E.list E.string role.tools )
         , ( "permissionLevel", E.string role.level )
         , ( "handoffTo", E.list E.string role.handoffTo )
         , ( "status", E.string role.status )
         , ( "evidence", E.string role.evidence )
         ]
            ++ List.filterMap identity
                [ Maybe.map (\v -> ( "sourceWorkflow", E.string v )) role.sourceWorkflow
                , Maybe.map
                    (\approval ->
                        ( "approvalBy"
                        , case approval of
                            Person uid ->
                                E.object [ ( "person", E.string uid ) ]

                            Permission permission ->
                                E.object [ ( "permission", E.string permission ) ]
                        )
                    )
                    role.approval
                ]
        )


encode : List Role -> E.Value
encode =
    E.list encodeRole
