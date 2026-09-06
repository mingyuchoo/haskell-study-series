module Ui.Form exposing (Config, checkValues, checks, formView, goalOptions, inputField, inputValue, peopleOptions, selectField, selectValue)

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
    label [] [ text label_, input [ name key, type_ kind, value (model.value action key), onInput (model.edit action key), required required_, step "any", autocomplete False ] [] ]


selectField : Config msg -> Action -> String -> String -> Bool -> List ( String, String ) -> Html msg
selectField model action label_ key required_ options =
    label [] [ text label_, select [ name key, value (model.value action key), onInput (model.edit action key), required required_ ] (List.map (\( key_, label__ ) -> option [ value key_, selected (model.value action key == key_) ] [ text label__ ]) options) ]


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
    label [] [ text label_, input [ name key, type_ kind, value current, onInput edit, required required_, step "any", autocomplete False ] [] ]


selectValue : String -> String -> (String -> msg) -> String -> Bool -> List ( String, String ) -> Html msg
selectValue key current edit label_ required_ options =
    label [] [ text label_, select [ name key, value current, onInput edit, required required_ ] (List.map (\( key_, label__ ) -> option [ value key_, selected (current == key_) ] [ text label__ ]) options) ]


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
