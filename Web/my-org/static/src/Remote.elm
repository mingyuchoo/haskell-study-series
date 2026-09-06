module Remote exposing (Remote(..), view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Ui.Common exposing (emptyState)


type Remote a
    = Loading
    | Loaded a
    | Failed String


view : Remote a -> (a -> Html msg) -> Html msg
view remote render =
    case remote of
        Loading ->
            section [ class "panel", attribute "role" "status" ] [ text "워크스페이스를 불러오는 중…" ]

        Failed message ->
            emptyState "조회하지 못했습니다" message

        Loaded data ->
            render data
