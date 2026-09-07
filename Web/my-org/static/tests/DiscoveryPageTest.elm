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
        , test "agent proposals display stored evidence, handoffs, approvals and unknown inputs" <|
            \_ ->
                let
                    doc =
                        D.empty
                            |> D.apply (D.AddWorkflow "w")
                            |> D.apply (D.WorkflowField "w" "name" "문의 접수")
                            |> D.apply (D.WorkflowField "w" "evidence" "인터뷰 메모")
                            |> D.apply (D.WorkflowField "w" "handoff" "재무팀에 전달")
                            |> D.apply (D.WorkflowField "w" "approval" "집행 전 팀장 승인")
                in
                Page.Discovery.view AgentDrafts (controls doc) workspace
                    |> Query.fromHtml
                    |> Query.has [ text "규칙 기반 제안 / 추론", text "근거: 인터뷰 메모", text "재무팀에 전달", text "집행 전 팀장 승인", text "미확인 · 확인 후 입력" ]
        , test "unsaved workflow edits cannot alter the displayed stored proposal" <|
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
                    [ \_ -> rendered |> Query.has [ text "저장된 업무 담당 에이전트 후보" ]
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
