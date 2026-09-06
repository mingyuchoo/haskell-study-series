module Page.Authorities exposing (view)

import Dict
import Domain exposing (..)
import Form.Action exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui.Common exposing (..)
import Ui.Form exposing (..)
import Ui.Label exposing (..)


type alias Controls msg =
    { forms : Config msg }


view : Controls msg -> { a | goals : List GoalView, people : List Person, decisionShare : Dict.Dict String Float, compiler : Compiler } -> Html msg
view model w =
    div []
        [ panel "책임을 실행할 수 있는 권한" [ note "권한을 줄여 활성 목표의 요건이 깨지면 해당 목표는 자동으로 초안으로 돌아갑니다.", note "집중도 = 보유 권한 종류 수 + 예산 보유 1점 / 조직 전체 점수. 실제 의사결정 빈도나 권력의 측정값은 아닙니다." ]
        , if List.isEmpty (List.filter .active w.people) then
            emptyState "구성원을 먼저 추가하세요" "구성원 메뉴에서 재직 구성원을 추가한 뒤 권한을 부여할 수 있습니다."

          else
            div [ class "grid" ] (List.map (\person -> section [ class "panel", id ("authority-" ++ person.id), tabindex -1 ] [ span [ class "tag" ] [ text ("권한 비중 " ++ String.fromInt (round (100 * (Dict.get person.id w.decisionShare |> Maybe.withDefault 0))) ++ "%") ], h2 [ class "form-heading" ] [ text person.name ], p [ class "muted" ] [ text person.role ], formView model.forms (Grant person.id) "권한 저장" [ inputField model.forms (Grant person.id) "집행 가능한 예산 한도 (KRW)" "budget" "number" True, checks model.forms (Grant person.id) ], note ("담당 목표 " ++ String.fromInt (List.length (List.filter (.owner >> (==) (Just person.id)) w.goals)) ++ "개") ]) (List.filter .active w.people))
        , diagnosticView w
        ]
