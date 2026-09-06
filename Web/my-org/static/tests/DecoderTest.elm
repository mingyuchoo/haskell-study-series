module DecoderTest exposing (tests)

import Api.Decode exposing (..)
import Domain exposing (..)
import Expect
import Json.Decode as D
import Test exposing (..)


tests : Test
tests =
    describe "HTTP JSON contract"
        [ test "legacy person defaults to active without optional profile" <|
            \_ ->
                D.decodeString personDecoder """{"id":"p","name":"Name","role":"Role"}"""
                    |> Expect.equal (Ok { id = "p", name = "Name", role = "Role", reportsTo = Nothing, department = Nothing, email = Nothing, active = True })
        , test "inactive person retains profile and report link" <|
            \_ ->
                D.decodeString personDecoder """{"id":"p","name":"Name","role":"Role","reportsTo":"boss","department":"Team","email":"a@example.com","status":"inactive"}"""
                    |> Expect.equal (Ok { id = "p", name = "Name", role = "Role", reportsTo = Just "boss", department = Just "Team", email = Just "a@example.com", active = False })
        , test "unknown employment status and malformed profile are rejected" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.err (D.decodeString personDecoder """{"id":"p","name":"Name","role":"Role","status":"unknown"}""")
                    , \_ -> Expect.err (D.decodeString personDecoder """{"id":"p","name":"Name","role":"Role","email":42}""")
                    ]
                    ()
        , test "optional measurement is absent or null, never silently mistyped" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal (Ok { status = NoData, progress = 0, latestValue = Nothing }) (D.decodeString evaluationDecoder "{\"status\":\"NoData\",\"progress\":0}")
                    , \_ -> Expect.equal (Ok Nothing) (D.decodeString (optional "latestValue" D.float) "{\"latestValue\":null}")
                    , \_ -> Expect.err (D.decodeString evaluationDecoder "{\"status\":\"NoData\",\"progress\":0,\"latestValue\":\"bad\"}")
                    ]
                    ()
        , test "unknown performance states are rejected" <|
            \_ -> Expect.err (D.decodeString statusDecoder "\"FutureState\"")
        , test "all known performance states decode" <|
            \_ -> List.map (\s -> D.decodeString statusDecoder ("\"" ++ s ++ "\"")) [ "NoData", "OnTrack", "AtRisk", "OffTrack", "Achieved" ] |> Expect.equal (List.map Ok [ NoData, OnTrack, AtRisk, OffTrack, Achieved ])
        , test "summary requires counts and organization metadata" <|
            \_ -> Expect.err (D.decodeString summaryDecoder "{\"organization\":{\"id\":\"o\",\"name\":\"A\",\"createdAt\":\"now\"},\"demo\":false,\"peopleCount\":0}")
        , test "workspace follows nested graph and compiler response contract" <|
            \_ ->
                D.decodeString workspaceDecoder """{"organization":{"id":"o","name":"A","createdAt":"now"},"version":2,"demo":false,"people":[],"goals":[],"authorities":[],"reviews":[],"compiler":{"errors":0,"warnings":0,"diagnostics":[]},"graph":{"edges":[]},"events":[],"decisionShare":{},"reviewWarnings":[]}"""
                    |> Result.map (\w -> ( w.organization.id, w.version, w.edges ))
                    |> Expect.equal (Ok ( "o", 2, [] ))
        , test "goal evaluation audit uses tagged event tuple" <|
            \_ ->
                D.decodeString auditDecoder """{"record":{"seq":3,"at":"now","actor":null,"event":{"tag":"GoalEvaluated","contents":["goal-1",{"status":"OnTrack"}]}},"description":"evaluated"}"""
                    |> Result.map (\a -> ( a.evaluatedGoal, a.evaluatedStatus ))
                    |> Expect.equal (Ok ( Just "goal-1", Just OnTrack ))
        , test "review learning objects and optional decision deadline decode" <|
            \_ ->
                D.decodeString reviewDecoder """{"id":"r","goal":"g","heldAt":"now","note":"review","evaluation":{"status":"NoData","progress":0},"learnings":[{"text":"learned"}],"decisions":[{"text":"next","owner":"p"}]}"""
                    |> Result.map (\r -> ( r.learnings, r.decisions ))
                    |> Expect.equal (Ok ( [ "learned" ], [ { text = "next", owner = "p", deadline = Nothing } ] ))
        ]
