module DiscoveryPageTest exposing (tests)

import App.Discovery as State
import Dict
import Domain.Discovery as D
import Expect
import Html.Attributes as Attr
import Page exposing (Page(..))
import Page.Discovery
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, tag, text)


workspace =
    { organization = { id = "a", name = "Synthetic team", createdAt = "2026-09-07" }, version = 1, demo = False, people = [], goals = [], authorities = [], reviews = [], compiler = { errors = 0, warnings = 0, diagnostics = [] }, edges = [], events = [], decisionShare = Dict.empty, reviewWarnings = [] }


controls doc =
    { state = State.init |> State.receive "a" (Ok { version = 1, discovery = doc }), org = "a", busy = False, edit = always (), addObservation = (), addWorkflow = (), save = (), rebase = (), reset = (), go = always () }


tests : Test
tests =
    describe "Discovery guidance and evidence"
        [ test "generic journey points to investigation without demo identities or activation requirements" <|
            \_ ->
                Page.Discovery.guide True () (always ()) D.empty
                    |> Query.fromHtml
                    |> Query.has [ text "미확인 사항은 남겨도 됩니다. 목표 활성화나 진단 해소는 현황 입력의 완료 조건이 아닙니다." ]
        , test "workflow references offer registered people, permissions and other workflows" <|
            \_ ->
                let
                    doc =
                        D.empty
                            |> D.apply (D.AddWorkflow "w")
                            |> D.apply (D.WorkflowField "w" "name" "문의 접수")
                            |> D.apply (D.AddWorkflow "x")
                            |> D.apply (D.WorkflowField "x" "name" "환불 검토")

                    withPeople =
                        { workspace | people = [ { id = "lead", name = "팀장", role = "고객지원 팀장", reportsTo = Nothing, department = Nothing, email = Nothing, active = True } ] }
                in
                Page.Discovery.view Workflows (controls doc) withPeople
                    |> Query.fromHtml
                    |> Query.has [ text "참조 연결 · 조직 데이터와 이어지는 정보", text "팀장 · 고객지원 팀장", text "가격 결정", tag "input", attribute (Attr.type_ "checkbox") ]
        , test "unsaved workflow edits keep the review confirmation disabled" <|
            \_ ->
                let
                    doc =
                        D.empty |> D.apply (D.AddWorkflow "w") |> D.apply (D.WorkflowField "w" "name" "저장된 업무")

                    initial =
                        controls doc

                    changed =
                        { initial | state = State.edit "a" (D.WorkflowField "w" "name" "미저장 업무") initial.state }

                    rendered =
                        Page.Discovery.view AgentDrafts changed workspace |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> rendered |> Query.has [ text "업무 또는 현황에 미저장 변경이 있습니다. 먼저 저장하면 새 근거를 바탕으로 검토할 수 있습니다." ]
                    , \_ -> rendered |> Query.find [ tag "input", attribute (Attr.type_ "checkbox") ] |> Query.has [ attribute (Attr.disabled True) ]
                    ]
                    ()
        , test "scope help is permanently associated with its input" <|
            \_ ->
                Page.Discovery.view Discovery (controls D.empty) workspace
                    |> Query.fromHtml
                    |> Query.find [ tag "textarea", attribute (Attr.id "discovery-scope") ]
                    |> Query.has [ attribute (Attr.attribute "aria-describedby" "discovery-scope-help discovery-scope-error") ]
        ]
