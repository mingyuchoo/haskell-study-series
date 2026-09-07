module Page.Agents exposing (Controls, graph, view)

import App.Agents as State
import Dict
import Domain exposing (Diagnostic, Workspace)
import Domain.Agent as A exposing (Change(..), Role)
import Domain.Discovery as D
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page exposing (Page(..))
import Ui.AgentGraph
import Ui.Common exposing (note, panel)
import Ui.Form exposing (guidedArea, guidedInput, peopleOptions, selectValue)
import Ui.Label exposing (permissionName, permissions, personName)


type alias Controls msg =
    { state : State.State
    , org : String
    , busy : Bool
    , edit : Change -> msg
    , importDrafts : msg
    , save : msg
    , rebase : msg
    , reset : msg
    , go : Page -> msg
    , exportHref : String
    , review : Html msg
    }


view : Controls msg -> Workspace -> Html msg
view controls workspace =
    case State.saved controls.org controls.state of
        Nothing ->
            panel "설계 불러오기" [ note (Dict.get controls.org controls.state.errors |> Maybe.withDefault "에이전트 설계를 불러오고 있습니다…") ]

        Just snapshot ->
            let
                design =
                    State.current controls.org controls.state |> Maybe.map .agents |> Maybe.withDefault snapshot.agents

                unsaved =
                    State.changed controls.org controls.state

                conflict =
                    State.conflicted controls.org controls.state

                unavailable =
                    controls.state.loading || Dict.member controls.org controls.state.errors
            in
            div []
                [ note "규칙 기반 초안은 저장된 업무 흐름에서 결정적으로 만듭니다. 실제 AI 실행, 도구 접근 권한 발급, 권한 부여는 하지 않습니다. 사람이 등급, 승인 주체, 인계 대상을 검토해 설계안으로 저장하세요."
                , drafts controls workspace snapshot
                , if unsaved then
                    p [ class "draft-notice", attribute "role" "status" ] [ text "저장하지 않은 설계안 입력이 있습니다. 화면 이동 시 유지되지만 브라우저를 닫거나 전체 새로고침하면 사라집니다." ]

                  else
                    text ""
                , if conflict then
                    panel "입력 중 조직이 변경되었습니다"
                        [ note "최신 저장 설계와 현재 입력을 비교하세요. 아래 버튼은 입력을 최신 버전에 다시 적용할 준비를 하며, 저장은 별도로 해야 합니다."
                        , button [ type_ "button", disabled (controls.busy || unavailable), onClick controls.rebase ] [ text "최신 내용 확인 후 내 입력 다시 적용" ]
                        ]

                  else
                    text ""
                , Html.form [ onSubmit controls.save ]
                    [ fieldset [ disabled (controls.busy || unavailable) ]
                        [ panel "사람이 검토한 설계안"
                            ([ note "각 역할의 권한 등급(L0 읽기, L1 작업 공간, L2 외부 영향, L3 금지), 사람 승인 주체, 허용 도구, 인계 대상을 확정합니다. 저장 후 서버가 같은 규칙으로 다시 진단합니다."
                             , div [ class "actions" ]
                                [ button [ type_ "button", id "agent-import", class "secondary", disabled (List.isEmpty snapshot.drafts), onClick controls.importDrafts ] [ text "규칙 기반 초안을 설계안으로 가져오기" ]
                                , if unsaved then
                                    button [ type_ "button", class "secondary", onClick controls.reset ] [ text "미저장 설계안 입력 취소" ]

                                  else
                                    text ""
                                ]
                             ]
                                ++ (if List.isEmpty design then
                                        [ note "아직 설계안이 없습니다. 초안을 가져온 뒤 검토하세요." ]

                                    else
                                        List.map (designCard controls workspace design) design
                                   )
                            )
                        , diagnostics "저장된 설계 진단" snapshot.diagnostics "저장된 설계에서 확인할 사항이 없습니다. 저장 전 입력은 저장 후 진단합니다."
                        , if List.isEmpty (A.problems design) then
                            text ""

                          else
                            div [ class "field-errors", attribute "role" "status" ] (List.map (\message -> p [] [ text message ]) (A.problems design))
                        , div [ class "actions" ]
                            [ button [ type_ "submit", disabled (unavailable || conflict || not (List.isEmpty (A.problems design))) ]
                                [ text
                                    (if controls.busy then
                                        "저장 중…"

                                     else
                                        "설계안 저장"
                                    )
                                ]
                            , a [ class "button-link secondary", href controls.exportHref, target "_blank", rel "noopener" ] [ text "정의 파일 내보내기 (Markdown)" ]
                            , button [ type_ "button", class "secondary", onClick (controls.go AgentGraph) ] [ text "에이전트 구조 보기 →" ]
                            ]
                        , note "내보내기는 저장된 설계를 사용하며, 저장된 설계가 없으면 규칙 기반 초안을 내보냅니다. 파일은 .claude/agents/<id>.md 형식의 정의 초안이며 실행 설정이 아닙니다."
                        ]
                    ]
                , controls.review
                ]


drafts controls workspace snapshot =
    panel "저장된 업무에서 도출한 역할 후보"
        ([ note "도출 규칙: 담당 역할이 후보 이름이 되고, 승인 조건이 있으면 L2, 도구가 있으면 L1, 그 외 L0입니다. 승인 주체와 인계 대상은 참조 연결을 우선 사용하고, 없으면 텍스트에 포함된 구성원 이름과 업무 이름으로 찾습니다. 비어 있는 정보는 미확인으로 남깁니다." ]
            ++ (if List.isEmpty snapshot.drafts then
                    [ note "아직 도출할 업무가 없습니다. 업무 흐름 화면에서 업무 이름과 알고 있는 내용을 입력하고 저장하세요." ]

                else
                    [ div [ class "grid" ] (List.map (draftCard workspace snapshot.drafts) snapshot.drafts) ]
               )
            ++ [ diagnostics "초안 진단" snapshot.draftDiagnostics "초안에서 확인할 사항이 없습니다." ]
        )


known value_ =
    if String.trim value_ == "" then
        "미확인 · 확인 후 입력"

    else
        value_


approvalText workspace approval =
    case approval of
        Just (A.Person uid) ->
            "구성원 " ++ personName workspace uid

        Just (A.Permission permission) ->
            permissionName permission ++ " 권한 보유자"

        Nothing ->
            "없음 또는 미확인"


handoffText roles targets =
    if List.isEmpty targets then
        "미확인"

    else
        String.join ", " (List.map (\t -> roles |> List.filter (.id >> (==) t) |> List.head |> Maybe.map .name |> Maybe.withDefault t) targets)


draftCard : Workspace -> List Role -> Role -> Html msg
draftCard workspace roles role =
    section [ class "panel agent-card", id ("draft-" ++ role.id) ]
        [ span [ class "tag" ] [ text "규칙 기반 제안 / 추론" ]
        , h2 [] [ text (role.name ++ " 에이전트 후보") ]
        , p [] [ text ("제안 이유: ‘" ++ role.task ++ "’의 입력을 받아 산출물을 만드는 역할 경계가 필요하기 때문입니다.") ]
        , note ("정보 구분: " ++ D.statusLabel role.status ++ " · 근거: " ++ known role.evidence)
        , dl []
            (List.concatMap (\( title, value_ ) -> [ dt [] [ text title ], dd [] [ text value_ ] ])
                [ ( "담당 업무", role.task )
                , ( "입력", known role.inputs )
                , ( "산출물", known role.outputs )
                , ( "도구 후보", known (A.toolsText role.tools) )
                , ( "권한 등급", A.levelLabel role.level )
                , ( "사람 승인", approvalText workspace role.approval )
                , ( "인계 대상", handoffText roles role.handoffTo )
                ]
            )
        ]


designCard : Controls msg -> Workspace -> List Role -> Role -> Html msg
designCard controls workspace roles role =
    let
        key suffix =
            "agent-" ++ role.id ++ "-" ++ suffix

        approvalOptions =
            ( "", "없음 또는 미확인" )
                :: List.map (\( uid, label_ ) -> ( "person:" ++ uid, "구성원 · " ++ label_ )) (List.drop 1 (peopleOptions workspace))
                ++ List.map (\( permission, label_ ) -> ( "permission:" ++ permission, "권한 보유자 · " ++ label_ )) permissions

        others =
            List.filter (\r -> r.id /= role.id) roles
    in
    section [ class "panel agent-card discovery-item", id ("design-" ++ role.id) ]
        [ h2 [] [ text (known role.name) ]
        , note ("도출 근거 업무: " ++ (role.sourceWorkflow |> Maybe.withDefault "없음") ++ " · 담당 업무: " ++ role.task)
        , div [ class "fields" ]
            [ guidedInput (key "name") "역할 이름" "에이전트가 맡는 책임을 이름으로 적으세요." "text" True role.name (controls.edit << Name role.id)
            , selectValue (key "level") role.level (controls.edit << Level role.id) "권한 등급" True A.levels
            , guidedArea (key "inputs") "입력" "이 역할이 받는 정보" False role.inputs (controls.edit << Inputs role.id)
            , guidedArea (key "outputs") "산출물" "이 역할이 만드는 결과물" False role.outputs (controls.edit << Outputs role.id)
            , guidedArea (key "tools") "허용 도구 후보" "쉼표로 구분합니다. 실제 접근 권한은 별도로 부여합니다." False (A.toolsText role.tools) (controls.edit << Tools role.id)
            , selectValue (key "approval") (A.approvalKey role.approval) (controls.edit << SetApproval role.id) "사람 승인 주체" False approvalOptions
            ]
        , if List.isEmpty others then
            note "인계 대상으로 연결할 다른 역할이 없습니다."

          else
            fieldset [ class "permission-fields" ]
                [ legend [] [ text "인계 대상 역할" ]
                , div [ class "checks" ]
                    (List.map
                        (\other ->
                            label []
                                [ input [ type_ "checkbox", checked (List.member other.id role.handoffTo), onCheck (controls.edit << Handoff role.id other.id) ] []
                                , text (known other.name)
                                ]
                        )
                        others
                    )
                ]
        , div [ class "fields" ]
            [ selectValue (key "status") role.status (controls.edit << Status role.id) "정보 구분" True [ ( "unknown", "미확인" ), ( "confirmed", "확인된 사실" ), ( "proposed", "개선안" ) ]
            , guidedArea (key "evidence") "근거" "확인된 사실이면 근거가 필요합니다." (role.status == "confirmed") role.evidence (controls.edit << Evidence role.id)
            ]
        , button [ type_ "button", class "secondary", onClick (controls.edit (Remove role.id)) ] [ text "이 역할 제외 · 저장 전 취소 가능" ]
        ]


diagnostics : String -> List Diagnostic -> String -> Html msg
diagnostics title items emptyText =
    panel title
        (if List.isEmpty items then
            [ note emptyText ]

         else
            List.map (\d -> div [ classList [ ( "diagnostic", True ), ( "error", d.severity == "Error" ) ] ] [ code [] [ text d.code ], strong [] [ text d.message ], p [] [ text ("역할: " ++ d.subject) ], div [] (List.map (\line -> p [] [ text line ]) d.details) ]) items
        )


{-| 저장된 설계가 있으면 설계를, 없으면 초안을 그린다.
-}
graph : State.State -> String -> Workspace -> Html msg
graph state org workspace =
    case State.saved org state of
        Nothing ->
            panel "설계 불러오기" [ note (Dict.get org state.errors |> Maybe.withDefault "에이전트 설계를 불러오고 있습니다…") ]

        Just snapshot ->
            let
                ( roles, items, source ) =
                    if List.isEmpty snapshot.agents then
                        ( snapshot.drafts, snapshot.draftDiagnostics, "규칙 기반 초안 (저장된 설계 없음)" )

                    else
                        ( snapshot.agents, snapshot.diagnostics, "저장된 설계" )
            in
            div []
                [ panel ("에이전트 인계 구조 · " ++ source)
                    [ note "역할 사이의 인계와 사람 승인 지점을 확인합니다. 조직 운영의 책임 그래프와 달리 이 구조는 설계 기록이며 실제 실행 경로가 아닙니다."
                    , Ui.AgentGraph.view workspace roles items
                    ]
                , diagnostics "구조 진단" items "확인할 사항이 없습니다."
                ]
