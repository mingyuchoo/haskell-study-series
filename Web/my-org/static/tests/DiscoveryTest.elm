module DiscoveryTest exposing (tests)

import App.Discovery as State
import Domain.Discovery as Discovery exposing (Change(..))
import Expect
import Test exposing (..)


tests : Test
tests =
    describe "Organization discovery drafts"
        [ test "receiving a newer server version preserves local evidence and requires explicit rebase" <|
            \_ ->
                let
                    initial =
                        State.init |> State.receive "a" (Ok { version = 7, discovery = Discovery.empty })

                    edited =
                        initial |> State.edit "a" (Scope "Local investigation")

                    received =
                        edited |> State.receive "a" (Ok { version = 9, discovery = Discovery.apply (Scope "Server scope") Discovery.empty })

                    rebased =
                        State.rebase "a" received
                in
                Expect.all
                    [ \_ -> Expect.equal (Just ( 7, "Local investigation" )) (State.current "a" received |> Maybe.map (\s -> ( s.version, s.discovery.scope )))
                    , \_ -> Expect.equal True (State.conflicted "a" received)
                    , \_ -> Expect.equal (Just ( 9, "Local investigation" )) (State.current "a" rebased |> Maybe.map (\s -> ( s.version, s.discovery.scope )))
                    , \_ -> Expect.equal False (State.conflicted "a" rebased)
                    ]
                    ()
        , test "organization drafts remain isolated through failure and refresh" <|
            \_ ->
                let
                    state =
                        State.init
                            |> State.receive "a" (Ok { version = 2, discovery = Discovery.empty })
                            |> State.receive "b" (Ok { version = 8, discovery = Discovery.empty })
                            |> State.edit "a" (Scope "Alpha")
                            |> State.edit "b" (Scope "Beta")
                            |> State.receive "a" (Err "offline")
                            |> State.receive "b" (Ok { version = 9, discovery = Discovery.empty })
                in
                Expect.equal ( Just "Alpha", Just "Beta" ) ( State.current "a" state |> Maybe.map (.discovery >> .scope), State.current "b" state |> Maybe.map (.discovery >> .scope) )
        , test "unknown workflow fields stay empty without inventing evidence" <|
            \_ ->
                let
                    doc =
                        Discovery.empty |> Discovery.apply (AddWorkflow "w") |> Discovery.apply (WorkflowField "w" "name" "Intake")
                in
                Expect.all
                    [ \_ -> Expect.equal [] (Discovery.problems doc)
                    , \_ -> Expect.equal (Just ( "unknown", "", "" )) (List.head doc.workflows |> Maybe.map (\w -> ( w.status, w.evidence, w.approval )))
                    , \_ -> doc |> Discovery.apply (WorkflowField "w" "status" "confirmed") |> Discovery.problems |> List.isEmpty |> Expect.equal False
                    ]
                    ()
        , test "source edits invalidate review while retaining review notes" <|
            \_ ->
                let
                    doc =
                        Discovery.empty |> Discovery.apply (ReviewNote "Check approvals") |> Discovery.apply (ReviewStatus "reviewed")
                in
                Expect.equal { status = "pending", note = "Check approvals" } (Discovery.apply (Scope "Changed scope") doc).review
        , test "confirmed observations require evidence and proposed work stays distinct" <|
            \_ ->
                let
                    doc =
                        Discovery.empty |> Discovery.apply (AddObservation "o") |> Discovery.apply (ObservationField "o" "subject" "Responsibility") |> Discovery.apply (ObservationField "o" "status" "confirmed")
                in
                Expect.all
                    [ \_ -> Expect.equal False (List.isEmpty (Discovery.problems doc))
                    , \_ -> doc |> Discovery.apply (ObservationField "o" "evidence" "Interview notes") |> Discovery.problems |> Expect.equal []
                    , \_ -> Expect.equal [ "확인된 사실", "미확인", "개선안" ] (List.map Discovery.statusLabel [ "confirmed", "unknown", "proposed" ])
                    ]
                    ()
        ]
