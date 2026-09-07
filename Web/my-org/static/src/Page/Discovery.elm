module Page.Discovery exposing (Controls, guide, view)

import App.Discovery as State
import Dict
import Domain exposing (Workspace)
import Domain.Discovery as D exposing (Change(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page exposing (Page(..))
import Ui.Common exposing (note, panel)
import Ui.Form exposing (guidedArea, guidedInput, peopleOptions, selectValue)
import Ui.Label exposing (permissions)


type alias Controls msg =
    { state : State.State, org : String, busy : Bool, edit : Change -> msg, addObservation : msg, addWorkflow : msg, save : msg, rebase : msg, reset : msg, go : Page -> msg }


view : Page -> Controls msg -> Workspace -> Html msg
view page controls workspace =
    case State.current controls.org controls.state of
        Nothing ->
            panel "현황 불러오기" [ note (Dict.get controls.org controls.state.errors |> Maybe.withDefault "조직 현황을 불러오고 있습니다…") ]

        Just snapshot ->
            let
                doc =
                    snapshot.discovery

                latest =
                    State.saved controls.org controls.state |> Maybe.map .discovery |> Maybe.withDefault D.empty

                unsaved =
                    State.changed controls.org controls.state

                conflict =
                    State.conflicted controls.org controls.state

                unavailable =
                    controls.state.loading || Dict.member controls.org controls.state.errors
            in
            div []
                [ note "확인된 사실에는 근거를 남기고, 모르는 내용은 미확인으로 보존하세요. 개선안은 현재 사실과 구분합니다. 이 입력은 운영 목표나 실제 권한을 자동으로 바꾸지 않습니다."
                , if unavailable then
                    note (Dict.get controls.org controls.state.errors |> Maybe.withDefault "최신 현황 확인 중 · 저장은 조회 완료 후 가능합니다.")

                  else
                    text ""
                , if unsaved then
                    p [ class "draft-notice", attribute "role" "status" ] [ text "저장하지 않은 입력이 있습니다. 화면 이동 시 유지되지만 브라우저를 닫거나 전체 새로고침하면 사라집니다. 에이전트 초안은 마지막 저장 내용을 사용합니다." ]

                  else
                    text ""
                , if conflict then
                    panel "입력 중 조직이 변경되었습니다"
                        [ note "최신 저장 내용과 현재 입력을 비교하세요. 아래 버튼은 입력을 최신 버전에 다시 적용할 준비를 하며, 저장은 별도로 해야 합니다."
                        , details [] [ summary [] [ text "최신 저장 내용 확인" ], pre [] [ text (documentSummary latest) ] ]
                        , button [ type_ "button", disabled (controls.busy || unavailable), onClick controls.rebase ] [ text "최신 내용 확인 후 내 입력 다시 적용" ]
                        ]

                  else
                    text ""
                , if unsaved then
                    details [] [ summary [] [ text "미저장 입력 되돌리기" ], note "현재 조직의 저장하지 않은 현황·업무·검토 입력 전체를 마지막 저장 내용으로 되돌립니다.", button [ type_ "button", class "secondary", disabled controls.busy, onClick controls.reset ] [ text "미저장 입력 취소" ] ]

                  else
                    text ""
                , Html.form [ onSubmit controls.save ]
                    [ fieldset [ disabled (controls.busy || unavailable) ]
                        [ case page of
                            Discovery ->
                                overview controls doc

                            Workflows ->
                                workflows controls workspace doc

                            _ ->
                                reviewForm doc latest controls
                        , if List.isEmpty (D.problems doc) then
                            text ""

                          else
                            div [ class "field-errors", attribute "role" "status" ] (List.map (\message -> p [] [ text message ]) (D.problems doc))
                        , div [ class "actions" ]
                            [ button [ type_ "submit", disabled (unavailable || conflict || not (List.isEmpty (D.problems doc))) ]
                                [ text
                                    (if controls.busy then
                                        "저장 중…"

                                     else if page == AgentDrafts then
                                        "검토 의견과 상태 저장"

                                     else
                                        "현황 저장"
                                    )
                                ]
                            , button
                                [ type_ "button"
                                , class "secondary"
                                , onClick
                                    (controls.go
                                        (if page == Discovery then
                                            Workflows

                                         else if page == Workflows then
                                            AgentDrafts

                                         else
                                            Responsibility
                                        )
                                    )
                                ]
                                [ text
                                    (if page == Discovery then
                                        "다음 · 업무 흐름 →"

                                     else if page == Workflows then
                                        "다음 · 에이전트 초안 →"

                                     else
                                        "책임 관계 살펴보기 →"
                                    )
                                ]
                            ]
                        ]
                    ]
                ]


overview controls doc =
    div []
        [ panel "1 · 현재 조직의 범위를 정하세요"
            [ note "누구의 어떤 시점 정보를 정리하는지 먼저 맞춥니다. 예: 고객지원팀의 9월 운영 현황"
            , guidedArea "discovery-scope" "분석 범위" "포함하는 팀·업무와 조사 목적을 적으세요. 예: 고객지원팀 문의 접수부터 해결까지" False doc.scope (controls.edit << Scope)
            , guidedInput "discovery-asof" "현황 기준일" "이 정보가 유효한 날짜입니다. 확인 전이면 비워 두세요." "date" False doc.asOf (controls.edit << AsOf)
            ]
        , panel "2 · 확인한 내용과 모르는 내용을 나누세요"
            ([ note "역할, 책임, 보고 관계, 결정 권한 등을 기록하세요. 예: 긴급 환불 승인자는 미확인 · 재무팀에 확인 예정. 권한 없음은 ‘없음’이라고 명시하고 근거를 남기세요." ]
                ++ List.map (observation controls) doc.observations
                ++ [ button [ type_ "button", class "secondary", onClick controls.addObservation ] [ text "+ 현황 항목 추가" ] ]
            )
        ]


observation controls item =
    section [ class "discovery-item" ]
        [ h3 []
            [ text
                (if item.subject == "" then
                    "새 현황 항목 · 미확인"

                 else
                    item.subject
                )
            ]
        , guidedInput (item.id ++ "-subject") "현황 항목" "예: 고객지원팀의 환불 승인 권한" "text" True item.subject (controls.edit << ObservationField item.id "subject")
        , guidedArea (item.id ++ "-detail") "내용" "현재 알고 있는 내용만 적으세요. 미확인은 부분 입력도 가능합니다." False item.detail (controls.edit << ObservationField item.id "detail")
        , statusField (item.id ++ "-status") item.status (controls.edit << ObservationField item.id "status")
        , guidedArea (item.id ++ "-evidence") "입력 근거 / 확인할 곳" (evidenceHint item.status) (item.status == "confirmed") item.evidence (controls.edit << ObservationField item.id "evidence")
        , button [ type_ "button", class "secondary", onClick (controls.edit (RemoveObservation item.id)) ] [ text "이 항목 제외 · 저장 전 취소 가능" ]
        ]


workflows controls workspace doc =
    div []
        [ panel "업무가 시작되어 다른 역할로 전달되는 흐름"
            [ note "한 업무가 시작되는 조건부터 입력, 도구, 산출물, 다음 전달 대상을 적습니다. 완벽히 알지 못해도 업무 이름과 미확인 상태로 시작할 수 있습니다."
            , note
                ("등록된 구성원 역할 참고: "
                    ++ (if List.isEmpty workspace.people then
                            "구성원 화면에서 현재 담당자를 등록할 수 있습니다."

                        else
                            String.join " · " (List.map (\p -> p.name ++ " / " ++ p.role) workspace.people)
                       )
                )
            , note "예: 문의 접수 → 고객지원 담당 → CRM 고객 정보 확인 → 답변 초안 → 환불 건은 재무 담당자의 승인 후 처리"
            ]
        , div [] (List.map (workflow controls workspace doc) doc.workflows)
        , button [ type_ "button", class "secondary", onClick controls.addWorkflow ] [ text "+ 업무 흐름 추가" ]
        ]


workflow controls workspace doc item =
    section [ class "panel discovery-item" ]
        [ h2 []
            [ text
                (if item.name == "" then
                    "새 업무 흐름"

                 else
                    item.name
                )
            ]
        , guidedInput (item.id ++ "-name") "업무 이름" "예: 고객 문의 분류와 답변" "text" True item.name (controls.edit << WorkflowField item.id "name")
        , div [ class "fields" ]
            (List.map (\( key, title, ( hint, value_ ) ) -> guidedArea (item.id ++ "-" ++ key) title hint False value_ (controls.edit << WorkflowField item.id key))
                [ ( "role", "현재 담당 역할 / 구성원", ( "직급보다 실제 책임을 적으세요. 예: 고객지원 담당 김민서", item.role ) )
                , ( "trigger", "시작 조건", ( "무엇이 발생하면 시작하나요? 예: 새 문의 접수", item.trigger ) )
                , ( "inputs", "입력 정보", ( "예: 문의 내용, 고객 계약 정보", item.inputs ) )
                , ( "tools", "현재 사용하는 도구", ( "예: CRM, 고객지원 문서. 실제 접근 권한은 별도 확인합니다.", item.tools ) )
                , ( "outputs", "산출물", ( "예: 문의 분류와 답변 초안", item.outputs ) )
                , ( "handoff", "전달 대상 / 인계 조건", ( "예: 환불 문의는 재무 담당자에게 금액과 사유 전달", item.handoff ) )
                , ( "approval", "사람의 승인 조건", ( "예: 환불 집행 전 재무 책임자 승인. 없음과 미확인을 구분하세요.", item.approval ) )
                ]
            )
        , references controls workspace doc item
        , statusField (item.id ++ "-status") item.status (controls.edit << WorkflowField item.id "status")
        , guidedArea (item.id ++ "-evidence") "입력 근거 / 확인할 곳" (evidenceHint item.status) (item.status == "confirmed") item.evidence (controls.edit << WorkflowField item.id "evidence")
        , button [ type_ "button", class "secondary", onClick (controls.edit (RemoveWorkflow item.id)) ] [ text "이 업무 제외 · 저장 전 취소 가능" ]
        ]


{-| 텍스트 설명과 별도로 구성원, 결정 권한, 다른 업무를 참조로 연결한다.
참조가 있으면 에이전트 초안이 텍스트 대신 참조를 사용한다.
-}
references controls workspace doc item =
    let
        others =
            List.filter (\w -> w.id /= item.id) doc.workflows

        permissionOptions =
            ( "", "권한 선택 안 함" ) :: permissions
    in
    fieldset [ class "form-section" ]
        [ legend [] [ text "참조 연결 · 조직 데이터와 이어지는 정보" ]
        , note "텍스트로 적은 담당자, 승인 조건, 인계 대상을 등록된 구성원, 결정 권한, 다른 업무에 연결합니다. 모르면 비워 두세요."
        , div [ class "fields" ]
            [ selectValue (item.id ++ "-role-person") (Maybe.withDefault "" item.rolePerson) (controls.edit << WorkflowRolePerson item.id) "담당 구성원" False (peopleOptions workspace)
            , selectValue (item.id ++ "-approval-person") (Maybe.withDefault "" item.approvalPerson) (controls.edit << WorkflowApprovalPerson item.id) "승인 구성원" False (peopleOptions workspace)
            , selectValue (item.id ++ "-approval-permission") (Maybe.withDefault "" item.approvalPermission) (controls.edit << WorkflowApprovalPermission item.id) "승인에 필요한 결정 권한" False permissionOptions
            ]
        , if List.isEmpty others then
            note "인계 대상으로 연결할 다른 업무가 아직 없습니다."

          else
            fieldset [ class "permission-fields" ]
                [ legend [] [ text "인계 대상 업무" ]
                , div [ class "checks" ]
                    (List.map
                        (\other ->
                            label []
                                [ input [ type_ "checkbox", checked (List.member other.id item.handoffWorkflows), onCheck (controls.edit << WorkflowHandoff item.id other.id) ] []
                                , text
                                    (if String.trim other.name == "" then
                                        "이름 없는 업무 (" ++ other.id ++ ")"

                                     else
                                        other.name
                                    )
                                ]
                        )
                        others
                    )
                ]
        ]


statusField key status edit =
    div [] [ selectValue key status edit "정보 구분" True [ ( "unknown", "미확인" ), ( "confirmed", "확인된 사실" ), ( "proposed", "개선안" ) ], note "확인된 사실: 근거가 있는 현재 정보 · 미확인: 추가 확인 필요 · 개선안: 앞으로 바꾸고 싶은 내용" ]


evidenceHint status =
    if status == "confirmed" then
        "필수: 문서명·확인한 담당자·확인 날짜 등 확인 가능한 근거를 적으세요."

    else
        "예: 9월 운영 매뉴얼, 담당자 인터뷰 또는 확인할 사람과 질문"


reviewForm doc latest controls =
    div []
        [ panel "사람의 검토와 수정 의견"
            [ note "초안과 설계안은 에이전트 초안 화면 위쪽에 있습니다. 여기서는 저장된 근거와 미확인 사항을 사람이 검토했는지 기록합니다."
            , note
                (if latest.review.status == "reviewed" then
                    "저장 상태: 검토 완료. 입력 근거가 바뀌면 다시 검토해야 합니다."

                 else
                    "저장 상태: 검토 대기. 역할 중복, 인계 누락과 사람 승인 조건을 확인하세요."
                )
            , guidedArea "agent-review-note" "검토 의견 / 수정할 제안" "예: 분류와 답변 역할을 분리하고 환불 실행은 사람 승인 후에만 허용" False doc.review.note (controls.edit << ReviewNote)
            , label [ class "review-confirmation" ]
                [ input
                    [ type_ "checkbox"
                    , checked (doc.review.status == "reviewed")
                    , disabled (sourceChanged doc latest || List.isEmpty latest.workflows)
                    , onCheck
                        (\checked_ ->
                            controls.edit
                                (ReviewStatus
                                    (if checked_ then
                                        "reviewed"

                                     else
                                        "pending"
                                    )
                                )
                        )
                    ]
                    []
                , text "저장된 근거와 미확인 사항을 검토했습니다"
                ]
            , if sourceChanged doc latest then
                note "업무 또는 현황에 미저장 변경이 있습니다. 먼저 저장하면 새 근거를 바탕으로 검토할 수 있습니다."

              else
                text ""
            ]
        ]


sourceChanged doc latest =
    ( doc.scope, doc.asOf, doc.observations ) /= ( latest.scope, latest.asOf, latest.observations ) || doc.workflows /= latest.workflows


documentSummary doc =
    String.join "\n" ([ "범위: " ++ doc.scope, "기준일: " ++ doc.asOf ] ++ List.map (\o -> o.subject ++ " / " ++ D.statusLabel o.status ++ " / " ++ o.detail ++ " / 근거: " ++ o.evidence) doc.observations ++ List.map (\w -> String.join " / " [ w.name, w.role, w.trigger, w.inputs, w.tools, w.outputs, w.handoff, w.approval, D.statusLabel w.status, w.evidence ]) doc.workflows ++ [ "검토: " ++ doc.review.status ++ " / " ++ doc.review.note ])


guide : Bool -> msg -> (Page -> msg) -> D.Document -> Html msg
guide expanded toggle go doc =
    let
        scopeDone =
            String.trim doc.scope /= "" && not (List.isEmpty doc.observations)

        workflowsDone =
            not (List.isEmpty doc.workflows)

        reviewed =
            workflowsDone && doc.review.status == "reviewed"

        next =
            if not scopeDone then
                Discovery

            else if not workflowsDone then
                Workflows

            else
                AgentDrafts
    in
    section [ class "journey-guide" ]
        [ div [ class "actions" ]
            [ strong [] [ text "현황 기록 → 업무 연결 → 에이전트 초안 검토" ]
            , button [ type_ "button", class "secondary", onClick (go next) ]
                [ text
                    (if reviewed then
                        "검토 완료 · 다시 살펴보기 →"

                     else
                        "다음 · " ++ Page.pageName next ++ " →"
                    )
                ]
            , button
                [ type_ "button"
                , class "secondary"
                , onClick toggle
                , attribute "aria-expanded"
                    (if expanded then
                        "true"

                     else
                        "false"
                    )
                ]
                [ text
                    (if expanded then
                        "단계 안내 접기"

                     else
                        "단계 안내 펼치기"
                    )
                ]
            ]
        , if expanded then
            div []
                [ note "진행 상태는 저장된 입력으로 계산합니다. 미확인 사항은 남겨도 됩니다. 목표 활성화나 진단 해소는 현황 입력의 완료 조건이 아닙니다."
                , ul []
                    (List.map
                        (\( done, title, page ) ->
                            li []
                                [ text
                                    ((if done then
                                        "입력됨 · "

                                      else
                                        "진행 전 · "
                                     )
                                        ++ title
                                        ++ " "
                                    )
                                , button [ type_ "button", class "secondary", onClick (go page) ] [ text "살펴보기" ]
                                ]
                        )
                        [ ( scopeDone, "1. 조직 범위와 사실·미확인 기록", Discovery ), ( workflowsDone, "2. 업무의 입력·산출물·인계 연결", Workflows ), ( reviewed, "3. 규칙 기반 제안을 사람이 검토", AgentDrafts ) ]
                    )
                ]

          else
            text ""
        ]
