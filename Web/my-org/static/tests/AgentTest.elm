module AgentTest exposing (tests)

import Api.Agents
import Api.Discovery
import App.Agents as State
import App.Effect exposing (Effect(..))
import App.Update exposing (Msg(..), update)
import AppFixture exposing (mapPage, mapSession)
import Dict
import Domain exposing (Workspace)
import Domain.Agent as Agent exposing (Approval(..), Change(..), Role)
import Domain.Discovery as Discovery
import Expect
import Form.Action
import Html
import Html.Attributes as Attr
import Json.Decode as D
import Json.Encode as E
import Page exposing (Page(..))
import Page.Agents
import Remote exposing (Remote(..))
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, tag, text)
import Ui.AgentGraph


workspace : Workspace
workspace =
    { organization = { id = "org-a", name = "Alpha", createdAt = "2026-01-01T00:00:00Z" }, version = 4, demo = False, people = [ { id = "lead", name = "팀장", role = "고객지원 팀장", reportsTo = Nothing, department = Nothing, email = Nothing, active = True } ], goals = [], authorities = [], reviews = [], compiler = { errors = 0, warnings = 0, diagnostics = [] }, edges = [], events = [], decisionShare = Dict.empty, reviewWarnings = [] }


role : String -> Role
role ident =
    { id = ident, name = "역할 " ++ ident, sourceWorkflow = Just ("w-" ++ ident), task = "업무 " ++ ident, inputs = "입력", outputs = "산출물", tools = [ "CRM" ], level = "L1", approval = Nothing, handoffTo = [], status = "confirmed", evidence = "인터뷰" }


intake : Role
intake =
    let
        base =
            role "intake"
    in
    { base | handoffTo = [ "refund" ] }


refund : Role
refund =
    let
        base =
            role "refund"
    in
    { base | level = "L2", approval = Just (Person "lead") }


snapshot : Agent.Snapshot
snapshot =
    { version = 11, agents = [], drafts = [ intake, refund ], diagnostics = [], draftDiagnostics = [ { severity = "Info", code = "A009", message = "산출물의 인계 대상이 미확인입니다.", subject = "refund", details = [] } ] }


ready : App.Update.Model
ready =
    App.Update.init { seed = "test", today = "2026-01-01", deadline = "2026-12-31" }
        |> Tuple.first
        |> mapSession (\s -> { s | org = Just "org-a", workspace = Loaded workspace, fresh = True, syncing = False })
        |> mapPage (\p -> { p | page = AgentDrafts })


step : Msg -> App.Update.Model -> App.Update.Model
step msg model =
    update msg model |> Tuple.first


controls : State.State -> Page.Agents.Controls ()
controls state =
    { state = state, org = "org-a", busy = False, edit = always (), importDrafts = (), save = (), rebase = (), reset = (), go = always (), exportHref = "/api/organizations/org-a/agents/export", review = Html.text "" }


tests : Test
tests =
    describe "Agent design"
        [ test "design edits keep handoffs consistent and removal drops references" <|
            \_ ->
                let
                    design =
                        [ intake, refund ]
                            |> Agent.apply (Level "intake" "L2")
                            |> Agent.apply (SetApproval "intake" "permission:Pricing")
                            |> Agent.apply (Tools "refund" "결제 콘솔, 장부\n메일")
                            |> Agent.apply (Handoff "refund" "refund" True)

                    removed =
                        Agent.apply (Remove "refund") design
                in
                Expect.all
                    [ \_ -> Expect.equal (Just ( "L2", Just (Permission "Pricing") )) (List.head design |> Maybe.map (\r -> ( r.level, r.approval )))
                    , \_ -> Expect.equal (Just [ "결제 콘솔", "장부", "메일" ]) (List.drop 1 design |> List.head |> Maybe.map .tools)
                    , \_ -> Expect.equal (Just []) (List.drop 1 design |> List.head |> Maybe.map .handoffTo)
                    , \_ -> Expect.equal [ ( "intake", [] ) ] (List.map (\r -> ( r.id, r.handoffTo )) removed)
                    , \_ -> Expect.equal [ "intake: 역할 이름을 입력하세요." ] (Agent.problems (Agent.apply (Name "intake" " ") [ intake ]))
                    , \_ -> Expect.equal [ "역할 intake: 확인된 사실에는 근거가 필요합니다." ] (Agent.problems (Agent.apply (Evidence "intake" "") [ intake ]))
                    ]
                    ()
        , test "snapshot decoding round-trips roles and diagnostics" <|
            \_ ->
                let
                    encoded =
                        E.object [ ( "version", E.int 11 ), ( "agents", E.list identity [] ), ( "drafts", Api.Agents.encode [ intake, refund ] ), ( "diagnostics", E.list identity [] ), ( "draftDiagnostics", E.list identity [ E.object [ ( "severity", E.string "Info" ), ( "code", E.string "A009" ), ( "message", E.string "산출물의 인계 대상이 미확인입니다." ), ( "subject", E.string "refund" ), ( "details", E.list identity [] ) ] ] ) ]
                in
                Expect.equal (Ok snapshot) (D.decodeValue Api.Agents.decoder encoded)
        , test "workflow references are omitted when empty and decoded when absent" <|
            \_ ->
                let
                    doc =
                        Discovery.empty |> Discovery.apply (Discovery.AddWorkflow "w")

                    keys =
                        D.decodeValue (D.field "workflows" (D.index 0 (D.keyValuePairs D.value |> D.map (List.map Tuple.first)))) (Api.Discovery.encode doc)

                    decoded =
                        D.decodeValue Api.Discovery.decoder (E.object [ ( "version", E.int 1 ), ( "discovery", Api.Discovery.encode doc ) ])
                in
                Expect.all
                    [ \_ -> Expect.equal (Ok False) (Result.map (List.member "handoffWorkflows") keys)
                    , \_ -> Expect.equal (Ok (Just ( Nothing, [] ))) (Result.map (.discovery >> .workflows >> List.head >> Maybe.map (\w -> ( w.rolePerson, w.handoffWorkflows ))) decoded)
                    ]
                    ()
        , test "importing drafts starts a local design that saves with the agents version" <|
            \_ ->
                let
                    loaded =
                        ready |> step (GotAgents ready.session.request "org-a" (Ok snapshot))

                    imported =
                        loaded |> step ImportAgentDrafts |> step (EditAgents (Level "intake" "L0"))
                in
                case update SubmitAgents imported |> Tuple.second of
                    [ SaveAgents _ "org-a" version agents ] ->
                        Expect.equal ( 11, [ "L0", "L2" ] ) ( version, List.map .level agents )

                    _ ->
                        Expect.fail "Expected exactly one scoped agents save"
        , test "a newer server version conflicts until the design is rebased" <|
            \_ ->
                let
                    state =
                        State.init
                            |> State.receive "org-a" (Ok snapshot)
                            |> State.edit "org-a" (Import snapshot.drafts)
                            |> State.receive "org-a" (Ok { snapshot | version = 12 })
                in
                Expect.all
                    [ \_ -> Expect.equal True (State.conflicted "org-a" state)
                    , \_ -> Expect.equal False (State.conflicted "org-a" (State.rebase "org-a" state))
                    , \_ -> Expect.equal (Just 2) (State.current "org-a" (State.rebase "org-a" state) |> Maybe.map (.agents >> List.length))
                    ]
                    ()
        , test "stale and cross-organization agent responses are ignored and deletion clears the design" <|
            \_ ->
                let
                    loaded =
                        ready |> step (GotAgents ready.session.request "org-a" (Ok snapshot)) |> step ImportAgentDrafts

                    deleted =
                        loaded |> step (Saved loaded.session.request Form.Action.DeleteOrg (Ok ()))
                in
                Expect.all
                    [ \_ -> Expect.equal ready (ready |> step (GotAgents 0 "org-a" (Ok snapshot)) |> step (GotAgents ready.session.request "org-b" (Ok snapshot)))
                    , \_ -> Expect.equal ( Nothing, Nothing ) ( State.current "org-a" deleted.agents, State.saved "org-a" deleted.agents )
                    ]
                    ()
        , test "the page shows rule-based drafts, diagnostics and an import action" <|
            \_ ->
                let
                    rendered =
                        Page.Agents.view (controls (State.init |> State.receive "org-a" (Ok snapshot))) workspace |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> rendered |> Query.has [ text "역할 refund 에이전트 후보", text "규칙 기반 제안 / 추론", text "구성원 팀장", text "L2 외부 영향 · 사람 승인 필요", text "A009" ]
                    , \_ -> rendered |> Query.find [ tag "a", attribute (Attr.href "/api/organizations/org-a/agents/export") ] |> Query.has [ text "정의 파일 내보내기 (Markdown)" ]
                    , \_ -> rendered |> Query.has [ text "규칙 기반 초안을 설계안으로 가져오기" ]
                    ]
                    ()
        , test "handoff layers order roles from sources to sinks and keep cycles" <|
            \_ ->
                let
                    cycleA =
                        { intake | id = "a", handoffTo = [ "b" ] }

                    cycleB =
                        { intake | id = "b", handoffTo = [ "a" ] }
                in
                Expect.all
                    [ \_ -> Expect.equal [ [ "intake" ], [ "refund" ] ] (List.map (List.map .id) (Ui.AgentGraph.layers [ refund, intake ]))
                    , \_ -> Expect.equal [ [ "a", "b" ] ] (List.map (List.map .id) (Ui.AgentGraph.layers [ cycleA, cycleB ]))
                    , \_ -> Ui.AgentGraph.view workspace [ intake, refund ] [] |> Query.fromHtml |> Query.has [ text "역할 intake", text "→ 인계 →", text "팀장 승인" ]
                    ]
                    ()
        ]
