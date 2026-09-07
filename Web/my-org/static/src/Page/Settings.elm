module Page.Settings exposing (view)

import Domain exposing (..)
import Form.Action exposing (..)
import Form.Snapshot exposing (Snapshot)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Ui.Common exposing (..)
import Ui.Form exposing (..)
import Ui.Label exposing (..)


type alias Controls msg =
    { forms : Config msg, deletion : Maybe Snapshot, goals : msg, openDelete : msg, closeDelete : msg, confirmDelete : String -> msg, noOp : msg }


view : Controls msg -> { a | organization : Organization, people : List Person, goals : List GoalView } -> Html msg
view model w =
    div []
        [ panel w.organization.name [ dl [ class "organization-meta" ] [ dt [] [ text "조직 ID" ], dd [] [ text w.organization.id ], dt [] [ text "등록일" ], dd [] [ text (String.left 10 w.organization.createdAt) ], dt [] [ text "구성원" ], dd [] [ text (String.fromInt (List.length w.people) ++ "명") ], dt [] [ text "목표" ], dd [] [ text (String.fromInt (List.length w.goals) ++ "개") ] ], button [ disabled model.forms.busy, onClick model.goals ] [ text "목표 →" ] ]
        , panel "조직 이름 수정" [ formView model.forms Rename "이름 저장" [ inputField model.forms Rename "조직 이름" "name" "text" True, note "구성원과 목표, 기존 기록을 유지합니다. 다른 변경과 충돌하면 최신 상태를 확인한 뒤 다시 저장하세요." ] ]
        , section [ class "panel danger-zone" ]
            [ h2 [] [ text "조직 삭제" ]
            , p [] [ text "조직 진단 기록, 업무 흐름, 에이전트 검토, 구성원, 목표, 책임, 권한, 결과, 평가, 회고와 전략이 현재 워크스페이스에서 제거됩니다." ]
            , note "논리 삭제입니다. 원본 감사 이벤트는 파일·DB에 보존되며 완전히 지워지지 않습니다. 다른 조직은 삭제되지 않습니다."
            , case model.deletion of
                Nothing ->
                    button [ class "danger-outline", disabled (model.forms.busy || not model.forms.fresh), onClick model.openDelete ] [ text "삭제 확인 열기…" ]

                Just snapshot ->
                    Html.form
                        [ onSubmit (model.forms.submit DeleteOrg)
                        , Html.Events.preventDefaultOn "keydown"
                            (D.field "key" D.string
                                |> D.map
                                    (\key ->
                                        if key == "Escape" then
                                            ( model.closeDelete, True )

                                        else
                                            ( model.noOp, False )
                                    )
                            )
                        , class "delete-confirmation"
                        , attribute "aria-labelledby" "delete-title"
                        ]
                        [ h3 [ id "delete-title" ] [ text (snapshot.name ++ " 조직을 삭제할까요?") ]
                        , fieldset [ disabled model.forms.busy ]
                            [ label [] [ text "확인하려면 조직 이름을 정확히 입력하세요", input [ id "delete-confirm", value snapshot.confirmation, onInput model.confirmDelete, autocomplete False, required True ] [] ]
                            , div [ class "actions" ]
                                [ button [ type_ "button", class "secondary", onClick model.closeDelete ] [ text "취소" ]
                                , button [ type_ "submit", class "danger", disabled (snapshot.confirmation /= snapshot.name || not model.forms.fresh) ]
                                    [ text
                                        (if model.forms.busy then
                                            "삭제 중…"

                                         else
                                            "조직 삭제"
                                        )
                                    ]
                                ]
                            ]
                        ]
            ]
        ]
