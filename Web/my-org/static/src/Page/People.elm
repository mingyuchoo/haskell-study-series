module Page.People exposing (matches, view)

import Domain exposing (..)
import Form.Action exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui.Common exposing (..)
import Ui.Form exposing (..)
import Ui.Label exposing (personName)


type alias Controls msg =
    { forms : Config msg, query : String, status : String, selected : Maybe String, search : String -> msg, filter : String -> msg, open : String -> msg, reset : String -> msg, goals : msg }


matches : String -> String -> Person -> Bool
matches query status person =
    (status == "all" || (status == "active" && person.active) || (status == "inactive" && not person.active))
        && String.contains (String.toLower (String.trim query)) (String.toLower (String.join " " [ person.name, person.role, Maybe.withDefault "" person.department, Maybe.withDefault "" person.email ]))


view : Controls msg -> Workspace -> Html msg
view model w =
    let
        people =
            List.filter (matches model.query model.status) w.people

        selected =
            w.people |> List.filter (\p -> Just p.id == model.selected) |> List.head
    in
    div []
        [ panel "구성원 관리"
            [ note "구성원의 기본정보와 보고 관계를 관리합니다. 비활성화하면 새 업무 배정에서 제외되며 과거 기록은 보존됩니다."
            , div [ class "fields" ]
                [ inputValue "people-search" model.query model.search "이름 · 역할 · 부서 · 이메일 검색" "search" False
                , selectValue "people-status" model.status model.filter "재직 상태" True [ ( "active", "재직" ), ( "inactive", "비활성" ), ( "all", "전체" ) ]
                ]
            , p [] [ text ("검색 결과 " ++ String.fromInt (List.length people) ++ "명 / 전체 " ++ String.fromInt (List.length w.people) ++ "명") ]
            , if List.isEmpty people then
                emptyState "표시할 구성원이 없습니다" "아래에서 구성원을 등록하거나 검색어와 재직 상태 필터를 변경하세요."

              else
                div [ class "grid" ] (List.map (personCard model w) people)
            ]
        , case selected of
            Just person ->
                detail model w person

            Nothing ->
                note "목록에서 ‘상세 · 수정’을 눌러 구성원 정보와 담당 목표를 확인하세요."
        , section [ class "panel", id "new-person", tabindex -1 ]
            [ h2 [] [ text "구성원 등록" ]
            , formView model.forms AddPerson "구성원 등록" (profileFields model.forms w AddPerson Nothing)
            ]
        ]


personCard : Controls msg -> Workspace -> Person -> Html msg
personCard model w person =
    article [ class "person-card goal-card" ]
        [ h3 [] [ text person.name ]
        , span [ class "tag" ]
            [ text
                (if person.active then
                    "재직"

                 else
                    "비활성"
                )
            ]
        , p [] [ text (person.role ++ " · " ++ Maybe.withDefault "부서 미입력" person.department) ]
        , p [ class "muted" ] [ text (Maybe.withDefault "이메일 미입력" person.email) ]
        , note ("담당 목표 " ++ String.fromInt (List.length (List.filter (.owner >> (==) (Just person.id)) w.goals)) ++ "개")
        , button [ class "secondary", disabled model.forms.busy, onClick (model.open person.id) ] [ text "상세 · 수정" ]
        ]


profileFields : Config msg -> Workspace -> Action -> Maybe String -> List (Html msg)
profileFields forms w action personId =
    [ div [ class "fields" ]
        [ inputField forms action "이름" "name" "text" True
        , inputField forms action "역할" "role" "text" True
        , inputField forms action "부서 (선택)" "department" "text" False
        , inputField forms action "이메일 (선택)" "email" "email" False
        , selectField forms
            action
            "보고 대상 (선택)"
            "reportsTo"
            False
            (( "", "없음" ) :: (w.people |> List.filter (\p -> p.active && Just p.id /= personId) |> List.map (\p -> ( p.id, p.name ++ " · " ++ p.role ))))
        ]
    ]


detail : Controls msg -> Workspace -> Person -> Html msg
detail model w person =
    let
        goals =
            List.filter (.owner >> (==) (Just person.id)) w.goals

        reports =
            List.filter (.reportsTo >> (==) (Just person.id)) w.people

        action =
            DeactivatePerson person.id

        requiresSuccessor =
            not (List.isEmpty goals && List.isEmpty reports)
    in
    section [ class "panel", id "person-detail", tabindex -1 ]
        [ h2 [] [ text (personName w person.id ++ " · 상세") ]
        , note ("구성원 ID: " ++ person.id)
        , note ("보고 대상: " ++ (person.reportsTo |> Maybe.map (personName w) |> Maybe.withDefault "없음"))
        , note
            ("직속 보고자: "
                ++ (if List.isEmpty reports then
                        "없음"

                    else
                        String.join ", " (List.map (\p -> personName w p.id) reports)
                   )
            )
        , button [ class "secondary", disabled (model.forms.busy || not model.forms.fresh), onClick (model.reset person.id) ] [ text "최신 정보로 다시 불러오기" ]
        , note "다시 불러오면 이 구성원의 저장하지 않은 기본정보와 인계 입력이 초기화됩니다."
        , formView model.forms (UpdatePerson person.id) "기본정보 저장" (profileFields model.forms w (UpdatePerson person.id) (Just person.id))
        , h3 [] [ text "담당 목표" ]
        , if List.isEmpty goals then
            note "현재 담당 목표가 없습니다."

          else
            ul [] (List.map (\g -> li [] [ text g.goal.description ]) goals)
        , button [ class "secondary", disabled model.forms.busy, onClick model.goals ] [ text "목표 관리 →" ]
        , if person.active then
            details [ class "person-deactivate" ]
                [ summary [] [ text "구성원 비활성화 · 업무 인계" ]
                , note ("담당 목표 " ++ String.fromInt (List.length goals) ++ "개와 직속 보고자 " ++ String.fromInt (List.length reports) ++ "명을 인계합니다. 연결된 업무 또는 보고자가 있으면 후임을 지정해야 합니다.")
                , note "인계한 목표는 초안으로 전환됩니다. 후임의 권한과 예산을 확인한 뒤 다시 활성화하세요. 기존 권한은 자동 복사되지 않습니다. 직속 보고자가 후임이면 기존 상위 보고자에게 연결됩니다."
                , note "과거 결과·회고·감사 기록은 기존 구성원을 유지합니다. 비활성화 후 신규 배정은 제한됩니다."
                , formView model.forms
                    action
                    "비활성화 및 인계 확정"
                    [ selectField model.forms
                        action
                        "후임 구성원"
                        "successor"
                        requiresSuccessor
                        (( ""
                         , if requiresSuccessor then
                            "후임 선택 (필수)"

                           else
                            "인계 대상 없음"
                         )
                            :: (w.people |> List.filter (\p -> p.active && p.id /= person.id) |> List.map (\p -> ( p.id, p.name ++ " · " ++ p.role )))
                        )
                    ]
                ]

          else
            note "비활성 구성원입니다. 기본정보를 수정하고 과거 기록을 조회할 수 있으며 새 업무를 배정할 수 없습니다."
        ]
