module StateTest exposing (tests)

import Dict
import Domain exposing (Workspace)
import Expect
import Form.Action exposing (..)
import Form.Goal
import Form.Review
import Http
import Json.Decode as D
import Main exposing (..)
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
    { initial | org = Just "org-a", page = Dashboard, workspace = Loaded workspace, fresh = True, syncing = False }


step : Msg -> Model -> Model
step msg model =
    update msg model |> Tuple.first


bodyField : D.Decoder a -> Model -> Action -> Result String a
bodyField decoder model action =
    payload model action |> Result.andThen (\( _, _, body ) -> D.decodeValue decoder body |> Result.mapError D.errorToString)


tests : Test
tests =
    describe "Application state safety"
        [ test "outdated workspace success and failure cannot replace current state" <|
            \_ ->
                ready |> step (GotWorkspace 0 (Ok { workspace | version = 99 })) |> step (GotWorkspace 0 (Err Http.NetworkError)) |> Expect.equal ready
        , test "outdated organization list is ignored" <|
            \_ -> Expect.equal ready (step (GotOrganizations 0 (Ok [])) ready)
        , test "current workspace response marks data fresh" <|
            \_ ->
                ready |> step Refresh |> step (GotWorkspace 2 (Ok { workspace | version = 5 })) |> (\m -> ( m.fresh, m.syncing, m.workspace )) |> Expect.equal ( True, False, Loaded { workspace | version = 5 } )
        , test "mutation blocks duplicate submit, edit, refresh and navigation" <|
            \_ ->
                let
                    saving =
                        step (Submit ImportDemo) ready
                in
                saving |> step (Submit ImportDemo) |> step (Navigate Organizations Nothing) |> step Refresh |> step (Edit AddPerson "name" "changed") |> Expect.equal saving
        , test "stale state refuses a new save" <|
            \_ ->
                ready |> step Refresh |> step (Submit ImportDemo) |> (\m -> ( m.saving, m.error, m.serial )) |> Expect.equal ( Idle, True, 0 )
        , test "drafts survive organization navigation independently" <|
            \_ ->
                ready |> step (Edit AddPerson "name" "Alice") |> step (Navigate Dashboard (Just "org-b")) |> step (Edit AddPerson "name" "Bob") |> step (Navigate Dashboard (Just "org-a")) |> (\m -> get m AddPerson "name") |> Expect.equal "Alice"
        , test "failed save preserves draft and metric ID while refreshing" <|
            \_ ->
                let
                    drafted =
                        step (EditGoal Form.Goal.Description "Revenue") ready

                    failed =
                        step (Saved ready.request AddGoal (Err "conflict")) drafted
                in
                Expect.equal ( ( "Revenue", get drafted AddGoal "metricId" ), ( Idle, False, True ) ) ( ( get failed AddGoal "description", get failed AddGoal "metricId" ), ( failed.saving, failed.fresh, failed.error ) )
        , test "rename refuses a version changed since last edit" <|
            \_ ->
                ready |> step (Edit Rename "name" "New") |> step (GotWorkspace ready.request (Ok { workspace | version = 5 })) |> (\m -> payload m Rename) |> Expect.err
        , test "re-editing rename captures current version" <|
            \_ ->
                ready |> step (Edit Rename "name" "New") |> step (GotWorkspace ready.request (Ok { workspace | version = 5 })) |> step (Edit Rename "name" "Newer") |> (\m -> bodyField (D.field "expectedVersion" D.int) m Rename) |> Expect.equal (Ok 5)
        , test "re-editing a refreshed person draft preserves the original version and refuses stale save" <|
            \_ ->
                let
                    w =
                        { workspace | people = [ { id = "p", name = "Original", role = "Original role", reportsTo = Nothing, department = Nothing, email = Nothing, active = True } ] }

                    m =
                        { ready | workspace = Loaded w }

                    freshWorkspace =
                        { w | version = 5, people = [ { id = "p", name = "Original", role = "Server changed role", reportsTo = Nothing, department = Nothing, email = Nothing, active = True } ] }
                in
                m |> step (Edit (UpdatePerson "p") "name" "Draft") |> step (GotWorkspace m.request (Ok freshWorkspace)) |> step (Edit (UpdatePerson "p") "name" "More edits") |> (\state -> payload state (UpdatePerson "p")) |> Expect.err
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
                        reset |> step (GotWorkspace reset.request (Ok w)) |> step (Edit (UpdatePerson "p") "name" "Fresh edit")
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
                    , \_ -> Expect.err (payload { confirmed | org = Just "org-b" } DeleteOrg)
                    ]
                    ()
        , test "metric ID changes only after successful goal creation" <|
            \_ ->
                let
                    drafted =
                        step (EditGoal Form.Goal.Description "Revenue") ready

                    unrelated =
                        step (Saved ready.request AddPerson (Ok ())) drafted

                    created =
                        step (Saved ready.request AddGoal (Ok ())) drafted
                in
                Expect.equal ( "metric-test-0", "metric-test-1" ) ( get unrelated AddGoal "metricId", get created AddGoal "metricId" )
        , test "guide preselects goal without losing review draft" <|
            \_ ->
                ready |> step (EditReview Form.Review.Note "Keep reflection") |> step (EditReview Form.Review.Learning "Keep learning") |> step (Guide Reviews "review-form") |> (\m -> ( get m AddReview "goal", get m AddReview "note", get m AddReview "learning" )) |> Expect.equal ( "demo-revenue", "Keep reflection", "Keep learning" )
        , test "guide preserves an explicitly selected review goal" <|
            \_ -> ready |> step (EditReview Form.Review.Goal "other-goal") |> step (Guide Reviews "review-form") |> (\m -> get m AddReview "goal") |> Expect.equal "other-goal"
        , test "review decision requires an owner" <|
            \_ -> ready |> step (EditReview Form.Review.Goal "g") |> step (EditReview Form.Review.Note "review") |> step (EditReview Form.Review.Decision "change") |> (\m -> payload m AddReview) |> Expect.err
        ]
