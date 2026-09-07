module Page.Organizations exposing (view, viewWith)

import Domain exposing (..)
import Form.Action exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Remote
import Ui.Common exposing (..)
import Ui.Form exposing (..)
import Ui.Label exposing (..)
import Ui.ListView as ListView exposing (Mode(..))


view : { a | forms : Config msg, organizations : Remote.Remote (List Summary), open : String -> msg, settings : String -> msg } -> Html msg
view =
    viewWith Table


viewWith : Mode -> { a | forms : Config msg, organizations : Remote.Remote (List Summary), open : String -> msg, settings : String -> msg } -> Html msg
viewWith mode model =
    div []
        [ panel "현재 조직을 이해하고 멀티 AI 에이전트 구조를 설계하세요"
            [ note "현재 조직의 역할·책임·업무 흐름을 기록하면, 저장된 근거로 에이전트 역할과 인계 구조의 초안을 검토할 수 있습니다."
            , note "1. 현재 사실과 미확인 내용을 기록 → 2. 업무의 입력·산출물·인계를 연결 → 3. 규칙 기반 에이전트 제안을 사람이 검토"
            , note "지금 확인할 수 있는 정보부터 시작하세요. 실제 AI 에이전트를 실행하거나 외부 도구의 권한을 부여하는 기능은 아닙니다."
            ]
        , panel "새 조직 등록"
            [ note "정리할 실제 조직이나 팀의 이름을 입력하세요. 예: 고객지원팀. 현황·업무·검토와 운영 기록은 조직별로 분리됩니다."
            , formView model.forms CreateOrg "조직 등록" [ inputField model.forms CreateOrg "조직 이름" "name" "text" True ]
            , button
                [ class "secondary"
                , disabled
                    (model.forms.busy
                        || not model.forms.fresh
                        || (case model.organizations of
                                Remote.Loaded items ->
                                    List.any (.organization >> .id >> (==) "demo-northstar-v2") items

                                _ ->
                                    True
                           )
                    )
                , onClick (model.forms.submit ImportDemo)
                ]
                [ text "체험용 데모 조직 추가" ]
            ]
        , Remote.view model.organizations
            (\items ->
                div []
                    [ div [ class "section-head" ] [ h2 [] [ text "등록된 조직" ], span [ class "tag" ] [ text (String.fromInt (List.length items) ++ "개") ] ]
                    , if List.isEmpty items then
                        emptyState "첫 조직을 시작하세요" "조직 이름을 등록한 뒤 조직 열기로 현황을 입력하세요. 데모 조직에서는 기존 목표·책임·권한 운영 흐름을 체험할 수 있습니다."

                      else if mode == Table then
                        organizationTable model items

                      else
                        div [ class "grid" ]
                            (List.map
                                (\item ->
                                    section [ class "panel organization-card" ]
                                        [ span [ class "tag" ]
                                            [ text
                                                (if item.demo then
                                                    "가상 데이터 · 데모"

                                                 else
                                                    "내 조직"
                                                )
                                            ]
                                        , h2 [] [ text item.organization.name ]
                                        , p [] [ text ("구성원 " ++ String.fromInt item.peopleCount ++ "명 · 목표 " ++ String.fromInt item.goalCount ++ "개") ]
                                        , small [] [ text ("등록 " ++ String.left 10 item.organization.createdAt) ]
                                        , div [ class "actions" ] [ button [ disabled model.forms.busy, onClick (model.open item.organization.id) ] [ text "조직 열기 →" ], button [ class "secondary", disabled model.forms.busy, onClick (model.settings item.organization.id) ] [ text "상세 · 수정 · 삭제" ] ]
                                        ]
                                )
                                items
                            )
                    ]
            )
        ]


organizationTable model items =
    ListView.tableView "등록된 조직"
        [ "조직명", "구분", "구성원 수", "목표 수", "등록일", "관리" ]
        (List.map
            (\item ->
                tr []
                    [ th [ scope "row" ] [ text item.organization.name ]
                    , td []
                        [ text
                            (if item.demo then
                                "가상 데이터 · 데모"

                             else
                                "내 조직"
                            )
                        ]
                    , td [] [ text (String.fromInt item.peopleCount ++ "명") ]
                    , td [] [ text (String.fromInt item.goalCount ++ "개") ]
                    , td [] [ text (String.left 10 item.organization.createdAt) ]
                    , td [] [ div [ class "actions" ] [ button [ disabled model.forms.busy, onClick (model.open item.organization.id) ] [ text "조직 열기 →" ], button [ class "secondary", disabled model.forms.busy, onClick (model.settings item.organization.id) ] [ text "상세 · 수정 · 삭제" ] ] ]
                    ]
            )
            items
        )
