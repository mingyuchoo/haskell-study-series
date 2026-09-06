module Ui.ListView exposing (Mode(..), controls, detailRow, tableView)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type Mode
    = Cards
    | Table


controls : Mode -> (Mode -> msg) -> Html msg
controls mode change =
    div [ class "list-view-toolbar", attribute "role" "group", attribute "aria-label" "목록 보기" ]
        [ span [] [ text "목록 보기" ]
        , button
            [ type_ "button"
            , classList [ ( "secondary", mode /= Cards ) ]
            , attribute "aria-pressed"
                (if mode == Cards then
                    "true"

                 else
                    "false"
                )
            , onClick (change Cards)
            ]
            [ text "카드" ]
        , button
            [ type_ "button"
            , classList [ ( "secondary", mode /= Table ) ]
            , attribute "aria-pressed"
                (if mode == Table then
                    "true"

                 else
                    "false"
                )
            , onClick (change Table)
            ]
            [ text "표" ]
        ]


tableView : String -> List String -> List (Html msg) -> Html msg
tableView title headers rows =
    div [ class "list-table-region" ]
        [ p [ class "note" ] [ text "화면이 좁으면 표 영역을 좌우로 스크롤하세요. 키보드는 표에 초점을 맞춘 뒤 방향키를 사용하세요." ]
        , div [ class "table-wrap list-table-wrap", tabindex 0, attribute "role" "region", attribute "aria-label" (title ++ " 표 · 좌우 스크롤") ]
            [ table [ class "list-table" ]
                [ caption [] [ text title ]
                , thead [] [ tr [] (List.map (\heading -> th [ scope "col" ] [ text heading ]) headers) ]
                , tbody [] rows
                ]
            ]
        ]


detailRow : Int -> List (Html.Attribute msg) -> String -> List (Html msg) -> Html msg
detailRow columns attrs title children =
    tr [ class "list-detail-row" ]
        [ td [ colspan columns ]
            [ details attrs [ summary [] [ text title ], div [ class "list-detail-content" ] children ] ]
        ]
