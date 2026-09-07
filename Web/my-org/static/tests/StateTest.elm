module StateTest exposing (tests)

import App.Discovery as DiscoveryState
import App.Effect exposing (Effect(..))
import App.Session exposing (SaveState(..))
import App.Update exposing (..)
import AppFixture exposing (mapPage, mapSession)
import Dict
import Domain exposing (Workspace)
import Domain.Discovery as Discovery
import Expect
import Form.Action exposing (..)
import Form.Goal
import Form.Review
import GraphFixture
import Json.Decode as D
import Page exposing (Page(..))
import Remote exposing (Remote(..))
import Test exposing (..)


workspace : Workspace
workspace =
    { organization = { id = "org-a", name = "Alpha", createdAt = "2026-01-01T00:00:00Z" }, version = 4, demo = False, people = [], goals = [], authorities = [], reviews = [], compiler = { errors = 0, warnings = 0, diagnostics = [] }, edges = [], events = [], decisionShare = Dict.empty, reviewWarnings = [] }


ready : Model
ready =
    let
        initial =
            init { seed = "test", today = "2026-01-01", deadline = "2026-12-31" } |> Tuple.first
    in
    initial |> mapSession (\s -> { s | org = Just "org-a", workspace = Loaded workspace, fresh = True, syncing = False }) |> mapPage (\p -> { p | page = Dashboard })


step : Msg -> Model -> Model
step msg model =
    update msg model |> Tuple.first


bodyField : D.Decoder a -> Model -> Action -> Result String a
bodyField decoder model action =
    payload model action |> Result.andThen (\( _, _, body ) -> D.decodeValue decoder body |> Result.mapError D.errorToString)


tests : Test
tests =
    describe "Application state safety"
        [ test "stale responses and duplicate or blocked actions produce no effects" <|
            \_ ->
                let
                    saving =
                        step (Submit ImportDemo) ready

                    stale =
                        step Refresh ready
                in
                Expect.all
                    [ \_ ->
                        List.map (\msg -> update msg ready |> Tuple.second)
                            [ GotWorkspace 0 (Ok workspace), GotWorkspace 0 (Err "offline"), GotOrganizations 0 (Ok []), Saved 0 ImportDemo (Ok ()), Saved 0 ImportDemo (Err "conflict") ]
                            |> Expect.equal [ [], [], [], [], [] ]
                    , \_ ->
                        List.map (\msg -> update msg saving |> Tuple.second)
                            [ Submit ImportDemo, Navigate Organizations Nothing, Refresh, Guide Reviews "review-form", OpenPerson "p", OpenDelete ]
                            |> Expect.equal [ [], [], [], [], [], [] ]
                    , \_ -> update (Submit ImportDemo) stale |> Tuple.second |> Expect.equal []
                    ]
                    ()
        , test "a valid submit emits exactly one scoped write with captured request and payload" <|
            \_ ->
                let
                    drafted =
                        ready |> step (Edit AddPerson "name" "Alice") |> step (Edit AddPerson "role" "Engineer")

                    ( saving, effects ) =
                        update (Submit AddPerson) drafted
                in
                case effects of
                    [ SaveCommand token action method path body ] ->
                        Expect.all
                            [ \_ -> Expect.equal ( ready.session.request, AddPerson ) ( token, action )
                            , \_ -> Expect.equal ( "POST", "/api/organizations/org-a/people" ) ( method, path )
                            , \_ -> D.decodeValue (D.map3 (\id name role -> ( id, name, role )) (D.field "id" D.string) (D.field "name" D.string) (D.field "role" D.string)) body |> Expect.equal (Ok ( "person-test-0", "Alice", "Engineer" ))
                            , \_ -> Expect.equal ( Saving "person", 1 ) ( saving.session.saving, saving.forms.serial )
                            ]
                            ()

                    _ ->
                        Expect.fail "Expected exactly one SaveCommand"
        , test "failed writes preserve drafts and emit only one read without retrying" <|
            \_ ->
                let
                    drafted =
                        ready |> step (Edit AddPerson "name" "Alice") |> step (Edit AddPerson "role" "Engineer")

                    saving =
                        step (Submit AddPerson) drafted

                    ( failed, effects ) =
                        update (Saved saving.session.request AddPerson (Err "conflict")) saving
                in
                Expect.equal ( "Alice", [ LoadWorkspace (saving.session.request + 1) "org-a", LoadDiscovery (saving.session.request + 1) "org-a", LoadAgents (saving.session.request + 1) "org-a" ], Idle ) ( get failed AddPerson "name", effects, failed.session.saving )
        , test "organization navigation emits the new scope and generation while same scope stays local" <|
            \_ ->
                let
                    ( changed, effects ) =
                        update (Navigate Dashboard (Just "org-b")) ready
                in
                Expect.all
                    [ \_ -> Expect.equal ( Just "org-b", ready.session.request + 1, [ LoadWorkspace (ready.session.request + 1) "org-b", LoadDiscovery (ready.session.request + 1) "org-b", LoadAgents (ready.session.request + 1) "org-b" ] ) ( changed.session.org, changed.session.request, effects )
                    , \_ -> update (Navigate Reviews (Just "org-a")) ready |> Tuple.second |> Expect.equal []
                    , \_ -> update (Navigate Organizations Nothing) changed |> Tuple.second |> Expect.equal [ LoadOrganizations (changed.session.request + 1) ]
                    , \_ -> init { seed = "test", today = "2026-01-01", deadline = "2026-12-31" } |> Tuple.second |> Expect.equal [ LoadOrganizations 1 ]
                    ]
                    ()
        , test "guides, person detail and delete confirmation emit their actual focus targets" <|
            \_ ->
                Expect.all
                    [ \_ -> update (Guide Reviews "review-form") ready |> Tuple.second |> Expect.equal [ FocusElement "review-form" ]
                    , \_ -> update (OpenPerson "p") ready |> Tuple.second |> Expect.equal [ FocusElement "person-detail" ]
                    , \_ -> update OpenDelete ready |> Tuple.second |> Expect.equal [ FocusElement "delete-confirm" ]
                    ]
                    ()
        , test "outdated workspace success and failure cannot replace current state" <|
            \_ ->
                ready |> step (GotWorkspace 0 (Ok { workspace | version = 99 })) |> step (GotWorkspace 0 (Err "network failure")) |> Expect.equal ready
        , test "outdated organization list is ignored" <|
            \_ -> Expect.equal ready (step (GotOrganizations 0 (Ok [])) ready)
        , test "current workspace response marks data fresh" <|
            \_ ->
                ready |> step Refresh |> step (GotWorkspace 2 (Ok { workspace | version = 5 })) |> (\m -> ( m.session.fresh, m.session.syncing, m.session.workspace )) |> Expect.equal ( True, False, Loaded { workspace | version = 5 } )
        , test "mutation blocks duplicate submit, edit, refresh and navigation" <|
            \_ ->
                let
                    saving =
                        step (Submit ImportDemo) ready
                in
                saving |> step (Submit ImportDemo) |> step (Navigate Organizations Nothing) |> step Refresh |> step (Edit AddPerson "name" "changed") |> Expect.equal saving
        , test "stale state refuses a new save" <|
            \_ ->
                ready |> step Refresh |> step (Submit ImportDemo) |> (\m -> ( m.session.saving, m.error, m.forms.serial )) |> Expect.equal ( Idle, True, 0 )
        , test "drafts survive organization navigation independently" <|
            \_ ->
                ready |> step (Edit AddPerson "name" "Alice") |> step (Navigate Dashboard (Just "org-b")) |> step (Edit AddPerson "name" "Bob") |> step (Navigate Dashboard (Just "org-a")) |> (\m -> get m AddPerson "name") |> Expect.equal "Alice"
        , test "failed save preserves draft and metric ID while refreshing" <|
            \_ ->
                let
                    drafted =
                        step (EditGoal Form.Goal.Description "Revenue") ready

                    failed =
                        step (Saved ready.session.request AddGoal (Err "conflict")) drafted
                in
                Expect.equal ( ( "Revenue", get drafted AddGoal "metricId" ), ( Idle, False, True ) ) ( ( get failed AddGoal "description", get failed AddGoal "metricId" ), ( failed.session.saving, failed.session.fresh, failed.error ) )
        , test "rename refuses a version changed since last edit" <|
            \_ ->
                ready |> step (Edit Rename "name" "New") |> step (GotWorkspace ready.session.request (Ok { workspace | version = 5 })) |> (\m -> payload m Rename) |> Expect.err
        , test "re-editing rename captures current version" <|
            \_ ->
                ready |> step (Edit Rename "name" "New") |> step (GotWorkspace ready.session.request (Ok { workspace | version = 5 })) |> step (Edit Rename "name" "Newer") |> (\m -> bodyField (D.field "expectedVersion" D.int) m Rename) |> Expect.equal (Ok 5)
        , test "re-editing a refreshed person draft preserves the original version and refuses stale save" <|
            \_ ->
                let
                    w =
                        { workspace | people = [ { id = "p", name = "Original", role = "Original role", reportsTo = Nothing, department = Nothing, email = Nothing, active = True } ] }

                    m =
                        mapSession (\s -> { s | workspace = Loaded w }) ready

                    freshWorkspace =
                        { w | version = 5, people = [ { id = "p", name = "Original", role = "Server changed role", reportsTo = Nothing, department = Nothing, email = Nothing, active = True } ] }
                in
                m |> step (Edit (UpdatePerson "p") "name" "Draft") |> step (GotWorkspace m.session.request (Ok freshWorkspace)) |> step (Edit (UpdatePerson "p") "name" "More edits") |> (\state -> payload state (UpdatePerson "p")) |> Expect.err
        , test "explicit person draft reset adopts current server fields and version" <|
            \_ ->
                let
                    w =
                        { workspace | version = 5, people = [ { id = "p", name = "Server", role = "Server role", reportsTo = Nothing, department = Nothing, email = Nothing, active = True } ] }

                    drafted =
                        ready |> step (Edit (UpdatePerson "p") "name" "Draft") |> step (Edit (DeactivatePerson "p") "successor" "other")

                    reset =
                        drafted |> step (ResetPerson "p")

                    loaded =
                        reset |> step (GotWorkspace reset.session.request (Ok w)) |> step (Edit (UpdatePerson "p") "name" "Fresh edit")
                in
                Expect.equal ( Ok 5, "Server role", "" ) ( bodyField (D.field "expectedVersion" D.int) loaded (UpdatePerson "p"), get loaded (UpdatePerson "p") "role", get loaded (DeactivatePerson "p") "successor" )
        , test "delete requires exact confirmation and captures the displayed version" <|
            \_ ->
                let
                    opened =
                        step OpenDelete ready

                    confirmed =
                        step (ConfirmDelete "Alpha") opened
                in
                Expect.all
                    [ \_ -> Expect.err (payload (step (ConfirmDelete "alpha") opened) DeleteOrg)
                    , \_ -> Expect.equal (Ok ( "Alpha", 4 )) (bodyField (D.map2 Tuple.pair (D.field "confirmName" D.string) (D.field "expectedVersion" D.int)) confirmed DeleteOrg)
                    , \_ -> Expect.err (payload (mapSession (\s -> { s | org = Just "org-b" }) confirmed) DeleteOrg)
                    ]
                    ()
        , test "metric ID changes only after successful goal creation" <|
            \_ ->
                let
                    drafted =
                        step (EditGoal Form.Goal.Description "Revenue") ready

                    unrelated =
                        step (Saved ready.session.request AddPerson (Ok ())) drafted

                    created =
                        step (Saved ready.session.request AddGoal (Ok ())) drafted
                in
                Expect.equal ( "metric-test-0", "metric-test-1" ) ( get unrelated AddGoal "metricId", get created AddGoal "metricId" )
        , test "guide preselects goal without losing review draft" <|
            \_ ->
                ready |> step (EditReview Form.Review.Note "Keep reflection") |> step (EditReview Form.Review.Learning "Keep learning") |> step (Guide Reviews "review-form") |> (\m -> ( get m AddReview "goal", get m AddReview "note", get m AddReview "learning" )) |> Expect.equal ( "demo-revenue", "Keep reflection", "Keep learning" )
        , test "guide preserves an explicitly selected review goal" <|
            \_ -> ready |> step (EditReview Form.Review.Goal "other-goal") |> step (Guide Reviews "review-form") |> (\m -> get m AddReview "goal") |> Expect.equal "other-goal"
        , test "review decision requires an owner" <|
            \_ -> ready |> step (EditReview Form.Review.Goal "g") |> step (EditReview Form.Review.Note "review") |> step (EditReview Form.Review.Decision "change") |> (\m -> payload m AddReview) |> Expect.err
        , test "discovery ignores another organization and an older request generation" <|
            \_ ->
                let
                    snapshot =
                        { version = 9, discovery = Discovery.empty }
                in
                Expect.equal ready (ready |> step (GotDiscovery 0 "org-a" (Ok snapshot)) |> step (GotDiscovery ready.session.request "org-b" (Ok snapshot)))
        , test "discovery saves using its own response version rather than dashboard version" <|
            \_ ->
                let
                    loaded =
                        ready |> step (GotDiscovery ready.session.request "org-a" (Ok { version = 11, discovery = Discovery.empty }))

                    edited =
                        loaded |> step (EditDiscovery (Discovery.Scope "Investigation"))
                in
                case update SubmitDiscovery edited |> Tuple.second of
                    [ SaveDiscovery _ "org-a" snapshot ] ->
                        Expect.equal ( 11, "Investigation" ) ( snapshot.version, snapshot.discovery.scope )

                    _ ->
                        Expect.fail "Expected a scoped discovery save"
        , test "successful save blocks discovery edits until the saved document is reloaded" <|
            \_ ->
                let
                    loaded =
                        ready |> step (GotDiscovery ready.session.request "org-a" (Ok { version = 11, discovery = Discovery.empty }))

                    saved =
                        loaded |> step (SavedDiscovery loaded.session.request "org-a" (Ok ()))

                    edited =
                        saved |> step (EditDiscovery (Discovery.Scope "Stale edit"))
                in
                Expect.equal saved edited
        , test "organization deletion removes both saved discovery and local drafts" <|
            \_ ->
                let
                    loaded =
                        ready |> step (GotDiscovery ready.session.request "org-a" (Ok { version = 11, discovery = Discovery.empty })) |> step (EditDiscovery (Discovery.Scope "Draft"))

                    deleted =
                        loaded |> step (Saved loaded.session.request DeleteOrg (Ok ()))
                in
                Expect.equal ( Nothing, Nothing ) ( DiscoveryState.current "org-a" deleted.discovery, DiscoveryState.saved "org-a" deleted.discovery )
        , test "choosing a shared metric copies its definition and choosing new creates a distinct identity" <|
            \_ ->
                let
                    loaded =
                        ready |> mapSession (\s -> { s | workspace = Loaded { workspace | goals = [ GraphFixture.goal ] } })

                    selected =
                        loaded |> step (EditGoal Form.Goal.MetricId "m")

                    fresh =
                        selected |> step (EditGoal Form.Goal.MetricId "")
                in
                Expect.all
                    [ \_ -> Expect.equal [ "m", "신규 고객 수", "명", "HigherIsBetter" ] (List.map (get selected AddGoal) [ "metricId", "metricName", "unit", "direction" ])
                    , \_ -> Expect.notEqual "m" (get fresh AddGoal "metricId")
                    , \_ -> Expect.equal "" (get fresh AddGoal "metricName")
                    ]
                    ()
        ]
