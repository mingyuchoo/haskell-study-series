module Page.Organizations exposing (view)

import Domain exposing (..)
import Form.Action exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Remote
import Ui.Common exposing (..)
import Ui.Form exposing (..)
import Ui.Label exposing (..)


view : { a | forms : Config msg, organizations : Remote.Remote (List Summary), open : String -> msg, settings : String -> msg } -> Html msg
view model =
    div []
        [ panel "새 조직 등록"
            [ note "각 조직의 구성원, 목표와 학습은 독립적으로 관리됩니다."
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
                        emptyState "첫 조직을 시작하세요" "조직 이름을 입력하거나 가상 데이터로 운영 흐름을 체험하세요."

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
