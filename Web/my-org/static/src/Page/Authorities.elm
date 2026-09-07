module Page.Authorities exposing (view, viewWith)

import Dict
import Domain exposing (..)
import Form.Action exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui.Common exposing (..)
import Ui.Form exposing (..)
import Ui.Label exposing (..)
import Ui.ListView as ListView exposing (Mode(..))


type alias Controls msg =
    { forms : Config msg }


view : Controls msg -> { a | goals : List GoalView, people : List Person, authorities : List Authority, decisionShare : Dict.Dict String Float, compiler : Compiler } -> Html msg
view =
    viewWith Table


viewWith : Mode -> Controls msg -> { a | goals : List GoalView, people : List Person, authorities : List Authority, decisionShare : Dict.Dict String Float, compiler : Compiler } -> Html msg
viewWith mode model w =
    div []
        [ panel "책임을 실행할 수 있는 권한" [ note "현재 실제로 행사할 수 있는 결정 권한과 예산 한도를 기록하세요. 예: 환불 승인 가능 / 채용 승인 불가. 모르는 권한은 조직 진단에 미확인으로 남깁니다. 권한을 줄여 활성 목표의 요건이 깨지면 초안으로 돌아갑니다.", note "집중도 = 보유 권한 종류 수 + 예산 보유 1점 / 조직 전체 점수. 실제 의사결정 빈도나 권력의 측정값은 아닙니다." ]
        , if List.isEmpty (List.filter .active w.people) then
            emptyState "구성원을 먼저 추가하세요" "구성원 메뉴에서 재직 구성원을 추가한 뒤 권한을 부여할 수 있습니다."

          else if mode == Table then
            ListView.tableView "구성원별 권한"
                [ "구성원", "역할", "현재 예산 한도", "보유 권한", "권한 비중", "담당 목표" ]
                (List.concatMap
                    (\person ->
                        [ tr [ id ("authority-" ++ person.id), tabindex -1 ]
                            [ th [ scope "row" ] [ text person.name ]
                            , td [] [ text person.role ]
                            , td [] [ text (savedBudget w person) ]
                            , td [] [ text (savedPermissions w person) ]
                            , td [] [ text (authorityShare w person) ]
                            , td [] [ text (goalCount w person) ]
                            ]
                        , ListView.detailRow 6 [] (person.name ++ " · 예산 · 권한 편집") [ authorityForm model person ]
                        ]
                    )
                    (List.filter .active w.people)
                )

          else
            div [ class "grid" ] (List.map (\person -> section [ class "panel", id ("authority-" ++ person.id), tabindex -1 ] [ span [ class "tag" ] [ text ("권한 비중 " ++ authorityShare w person) ], h2 [ class "form-heading" ] [ text person.name ], p [ class "muted" ] [ text person.role ], authorityForm model person, note ("담당 목표 " ++ goalCount w person) ]) (List.filter .active w.people))
        , diagnosticView w
        ]


authorityForm model person =
    formView model.forms (Grant person.id) "권한 저장" [ inputField model.forms (Grant person.id) "현재 집행 가능한 예산 한도 (KRW)" "budget" "number" True, checks model.forms (Grant person.id) ]


authorityShare w person =
    String.fromInt (round (100 * (Dict.get person.id w.decisionShare |> Maybe.withDefault 0))) ++ "%"


goalCount w person =
    String.fromInt (List.length (List.filter (.owner >> (==) (Just person.id)) w.goals)) ++ "개"


savedAuthority : { a | authorities : List Authority } -> Person -> Maybe Authority
savedAuthority w person =
    List.filter (.owner >> (==) person.id) w.authorities |> List.head


savedBudget : { a | authorities : List Authority } -> Person -> String
savedBudget w person =
    savedAuthority w person |> Maybe.map (\a -> formatNumber a.budgetLimit ++ "원") |> Maybe.withDefault "미설정"


savedPermissions : { a | authorities : List Authority } -> Person -> String
savedPermissions w person =
    case savedAuthority w person of
        Nothing ->
            "없음"

        Just authority ->
            let
                labels =
                    permissions
                        |> List.filter (\( key, _ ) -> List.member key authority.canApprove || (key == "Hiring" && authority.canHire) || (key == "Pricing" && authority.canChangePrice))
                        |> List.map Tuple.second
            in
            if List.isEmpty labels then
                "없음"

            else
                String.join " · " labels
