module Ui.Form exposing (Config, checkValues, checks, formView, goalOptions, guidedArea, guidedInput, inputField, inputValue, peopleOptions, selectField, selectValue)

import Domain exposing (..)
import Form.Action exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui.Label exposing (permissions)


type alias Config msg =
    { busy : Bool, fresh : Bool, saving : Maybe String, value : Action -> String -> String, edit : Action -> String -> String -> msg, submit : Action -> msg }


formView : Config msg -> Action -> String -> List (Html msg) -> Html msg
formView model action label_ children =
    Html.form [ onSubmit (model.submit action) ]
        [ fieldset [ disabled model.busy ]
            (children
                ++ [ button [ type_ "submit", disabled (not model.fresh) ]
                        [ text
                            (if model.saving == Just (actionKey action) then
                                "저장 중…"

                             else
                                label_
                            )
                        ]
                   ]
            )
        ]


inputField : Config msg -> Action -> String -> String -> String -> Bool -> Html msg
inputField model action label_ key kind required_ =
    guidedInputNamed (actionKey action ++ "-" ++ key) key label_ (help key) kind required_ (model.value action key) (model.edit action key)


selectField : Config msg -> Action -> String -> String -> Bool -> List ( String, String ) -> Html msg
selectField model action label_ key required_ options =
    selectWithHelp (actionKey action ++ "-" ++ key) key (model.value action key) (model.edit action key) label_ required_ options


checks : Config msg -> Action -> Html msg
checks model action =
    fieldset [ class "permission-fields" ]
        [ legend [] [ text "결정 권한" ]
        , div [ class "checks" ]
            (List.map
                (\( key, label_ ) ->
                    label []
                        [ input
                            [ type_ "checkbox"
                            , checked (model.value action key == "true")
                            , onCheck
                                (\checked_ ->
                                    model.edit action
                                        key
                                        (if checked_ then
                                            "true"

                                         else
                                            "false"
                                        )
                                )
                            ]
                            []
                        , text label_
                        ]
                )
                permissions
            )
        ]


peopleOptions : { a | people : List Person } -> List ( String, String )
peopleOptions w =
    ( "", "구성원 선택" ) :: List.map (\p -> ( p.id, p.name ++ " · " ++ p.role )) (List.filter .active w.people)


goalOptions : { a | goals : List GoalView } -> List ( String, String )
goalOptions w =
    ( "", "목표 선택" ) :: List.map (\g -> ( g.goal.id, g.goal.description )) w.goals


inputValue : String -> String -> (String -> msg) -> String -> String -> Bool -> Html msg
inputValue key current edit label_ kind required_ =
    guidedInput key label_ (help key) kind required_ current edit


selectValue : String -> String -> (String -> msg) -> String -> Bool -> List ( String, String ) -> Html msg
selectValue key current edit label_ required_ options =
    selectWithHelp key key current edit label_ required_ options


checkValues : (String -> String) -> (String -> String -> msg) -> Html msg
checkValues current edit =
    fieldset [ class "permission-fields" ]
        [ legend [] [ text "결정 권한" ]
        , div [ class "checks" ]
            (List.map
                (\( key, label_ ) ->
                    label []
                        [ input
                            [ type_ "checkbox"
                            , checked (current key == "true")
                            , onCheck
                                (\checked_ ->
                                    edit key
                                        (if checked_ then
                                            "true"

                                         else
                                            "false"
                                        )
                                )
                            ]
                            []
                        , text label_
                        ]
                )
                permissions
            )
        ]


help : String -> String
help key =
    case key of
        "name" ->
            "실제 조직 또는 구성원의 이름을 입력하세요."

        "role" ->
            "현재 맡은 역할을 적으세요. 예: 고객지원 운영 책임자"

        "department" ->
            "현재 소속된 부서를 적으세요. 확인 전이면 비워 두어도 됩니다."

        "reportsTo" ->
            "현재 업무 보고를 받는 사람입니다. 먼저 구성원을 등록한 뒤 연결할 수 있습니다."

        "owner" ->
            "함께 일하는 모두가 아니라 결과에 대해 최종 판단과 설명을 맡는 한 명입니다."

        "description" ->
            "현재 관리 중인 결과를 적으세요. 예: 고객 문의 응답 시간 단축"

        "metricName" ->
            "결과를 확인하는 지표입니다. 예: 평균 첫 응답 시간"

        "unit" ->
            "숫자의 단위를 적으세요. 예: 분, 건, %, 원"

        "baseline" ->
            "측정 시작 시점의 값입니다. 예: 평균 첫 응답 60분"

        "target" ->
            "마감까지 도달하려는 값입니다. 예: 평균 첫 응답 30분"

        "direction" ->
            "매출은 높을수록, 응답 시간은 낮을수록 좋은 지표입니다."

        "startsAt" ->
            "측정이 시작되는 날짜입니다. 날짜는 UTC 기준으로 저장합니다."

        "deadline" ->
            "목표 달성 기한입니다. 시작일보다 앞설 수 없습니다."

        "budget" ->
            "원(KRW) 단위로 숫자만 입력하세요. 0은 예산 없음이며 미확인과 다릅니다."

        "parent" ->
            "이 목표가 기여하는 상위 목표입니다. 지표가 자동 합산되지는 않습니다."

        "reportedBy" ->
            "실제 측정값을 확인하고 보고한 구성원입니다."

        "value" ->
            "목표에 표시된 KPI 단위로 실제 측정한 수치를 입력하세요."

        "note" ->
            "확인한 내용과 근거를 적으세요. 예: 9월 고객지원 보고서에서 확인"

        "decision" ->
            "학습을 바탕으로 바꿀 행동입니다. 예: 긴급 문의는 당일 담당자에게 전달"

        "decisionOwner" ->
            "다음 결정을 실행하고 완료 여부를 확인할 사람입니다."

        "decisionDeadline" ->
            "결정 실행 기한입니다. 날짜는 UTC 기준입니다."

        "email" ->
            "연락처를 참고하기 위한 선택 정보입니다. 예: member@example.com"

        _ ->
            ""


fieldError kind required_ current =
    if required_ && String.trim current == "" then
        "필수 항목입니다. 내용을 입력하세요."

    else if kind == "number" && current /= "" && String.toFloat current == Nothing then
        "숫자로 입력하세요."

    else
        ""


guidedInput : String -> String -> String -> String -> Bool -> String -> (String -> msg) -> Html msg
guidedInput key title hint kind required_ current edit =
    guidedInputNamed key key title hint kind required_ current edit


guidedInputNamed key name_ title hint kind required_ current edit =
    let
        error =
            fieldError kind required_ current
    in
    label [ for key, class "guided-field" ]
        [ span []
            [ text
                (title
                    ++ (if required_ then
                            " · 필수"

                        else
                            ""
                       )
                )
            ]
        , input
            [ id key
            , name key
            , type_ kind
            , value current
            , onInput edit
            , required required_
            , step "any"
            , autocomplete False
            , attribute "aria-describedby" (key ++ "-help " ++ key ++ "-error")
            , attribute "aria-invalid"
                (if error == "" then
                    "false"

                 else
                    "true"
                )
            ]
            []
        , small [ id (key ++ "-help"), class "field-help" ] [ text hint ]
        , small [ id (key ++ "-error"), class "field-error", attribute "aria-live" "polite" ] [ text error ]
        ]


guidedArea : String -> String -> String -> Bool -> String -> (String -> msg) -> Html msg
guidedArea key title hint required_ current edit =
    let
        error =
            fieldError "text" required_ current
    in
    label [ for key, class "guided-field" ]
        [ span []
            [ text
                (title
                    ++ (if required_ then
                            " · 필수"

                        else
                            ""
                       )
                )
            ]
        , textarea
            [ id key
            , name key
            , value current
            , onInput edit
            , required required_
            , rows 3
            , attribute "aria-describedby" (key ++ "-help " ++ key ++ "-error")
            , attribute "aria-invalid"
                (if error == "" then
                    "false"

                 else
                    "true"
                )
            ]
            []
        , small [ id (key ++ "-help"), class "field-help" ] [ text hint ]
        , small [ id (key ++ "-error"), class "field-error", attribute "aria-live" "polite" ] [ text error ]
        ]


selectWithHelp key name_ current edit title required_ options =
    label [ for key, class "guided-field" ]
        [ span [] [ text title ]
        , select [ id key, name name_, value current, onInput edit, required required_, attribute "aria-describedby" (key ++ "-help") ] (List.map (\( ident, label_ ) -> option [ value ident, selected (current == ident) ] [ text label_ ]) options)
        , small [ id (key ++ "-help"), class "field-help" ] [ text (help name_) ]
        ]
