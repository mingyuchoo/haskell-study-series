module App.Drafts exposing (Context, State, advanceSerial, closeDelete, confirmDelete, draftDefaults, edit, editGoal, editReview, formKey, get, goalDraft, init, openDelete, payload, prepareReview, removeOrganization, resetPerson, reviewDraft, saved, workspaceVersion)

import Api.Command
import App.Config exposing (Flags)
import App.Session as Session
import Dict exposing (Dict)
import Form.Action exposing (..)
import Form.Defaults
import Form.Goal
import Form.Review
import Form.Snapshot exposing (Snapshot)
import Json.Encode as E
import Remote exposing (Remote(..))


type alias State =
    { drafts : Dict String (Dict String String)
    , goalDrafts : Dict String Form.Goal.Draft
    , reviewDrafts : Dict String Form.Review.Draft
    , serial : Int
    , goalSerial : Dict String Int
    , deletion : Maybe Snapshot
    }


type alias Context a =
    { a | session : Session.State, forms : State, flags : Flags }


init : State
init =
    { drafts = Dict.empty, goalDrafts = Dict.empty, reviewDrafts = Dict.empty, serial = 0, goalSerial = Dict.empty, deletion = Nothing }


formKey : Context a -> Action -> String
formKey model action =
    Maybe.withDefault "list" model.session.org ++ "/" ++ actionKey action


get : Context a -> Action -> String -> String
get model action name =
    case action of
        AddGoal ->
            Form.Goal.fromKey name |> Maybe.map (Form.Goal.value (goalDraft model)) |> Maybe.withDefault ""

        AddReview ->
            Form.Review.fromKey name |> Maybe.map (Form.Review.value (reviewDraft model)) |> Maybe.withDefault ""

        _ ->
            Dict.get (formKey model action) model.forms.drafts |> Maybe.andThen (Dict.get name) |> Maybe.withDefault (defaultValue model action name)


workspaceVersion : Context a -> Int
workspaceVersion model =
    case model.session.workspace of
        Loaded w ->
            w.version

        _ ->
            0


goalDraft : Context a -> Form.Goal.Draft
goalDraft model =
    Dict.get (formKey model AddGoal) model.forms.goalDrafts |> Maybe.withDefault (Form.Goal.fromValues (defaultValue model AddGoal))


reviewDraft : Context a -> Form.Review.Draft
reviewDraft model =
    Dict.get (formKey model AddReview) model.forms.reviewDrafts |> Maybe.withDefault (Form.Review.fromValues (defaultValue model AddReview))


payload : Context a -> Action -> Result String ( String, String, E.Value )
payload model action =
    Api.Command.payload { org = model.session.org, seed = model.flags.seed, serial = model.forms.serial, version = workspaceVersion model, deletion = model.forms.deletion, value = get model, goal = goalDraft model, review = reviewDraft model } action


defaultContext : Context a -> Form.Defaults.Context
defaultContext model =
    { seed = model.flags.seed
    , today = model.flags.today
    , deadline = model.flags.deadline
    , goalIndex = Dict.get (Maybe.withDefault "" model.session.org) model.forms.goalSerial |> Maybe.withDefault 0
    , workspace =
        case model.session.workspace of
            Loaded data ->
                Just data

            _ ->
                Nothing
    }


defaultValue : Context a -> Action -> String -> String
defaultValue model =
    Form.Defaults.defaultValue (defaultContext model)


draftDefaults : Context a -> Action -> Dict String String
draftDefaults model =
    Form.Defaults.draftDefaults (defaultContext model)


edit : Action -> String -> String -> Context a -> State
edit action key value model =
    let
        state =
            model.forms

        draftKey =
            formKey model action

        current =
            Dict.get draftKey state.drafts |> Maybe.withDefault (draftDefaults model action)

        version =
            case action of
                UpdatePerson _ ->
                    Dict.get "__version" current |> Maybe.withDefault (String.fromInt (workspaceVersion model))

                DeactivatePerson _ ->
                    Dict.get "__version" current |> Maybe.withDefault (String.fromInt (workspaceVersion model))

                _ ->
                    String.fromInt (workspaceVersion model)
    in
    { state | drafts = Dict.insert draftKey (Dict.insert "__version" version (Dict.insert key value current)) state.drafts }


saved : Action -> Context a -> State
saved action model =
    let
        state =
            model.forms

        key =
            formKey model action
    in
    { state
        | goalSerial =
            if action == AddGoal then
                Dict.update (Maybe.withDefault "" model.session.org) (Maybe.withDefault 0 >> (+) 1 >> Just) state.goalSerial

            else
                state.goalSerial
        , drafts = Dict.remove key state.drafts
        , goalDrafts =
            if action == AddGoal then
                Dict.remove key state.goalDrafts

            else
                state.goalDrafts
        , reviewDrafts =
            if action == AddReview then
                Dict.remove key state.reviewDrafts

            else
                state.reviewDrafts
        , deletion = Nothing
    }


removeOrganization : Maybe String -> State -> State
removeOrganization org state =
    let
        keep key _ =
            not (String.startsWith (Maybe.withDefault "" org ++ "/") key)
    in
    { state | drafts = Dict.filter keep state.drafts, goalDrafts = Dict.filter keep state.goalDrafts, reviewDrafts = Dict.filter keep state.reviewDrafts }


editGoal : Form.Goal.Field -> String -> Context a -> State
editGoal field value model =
    let
        state =
            model.forms
    in
    { state | goalDrafts = Dict.insert (formKey model AddGoal) (Form.Goal.edit field value (goalDraft model)) state.goalDrafts }


editReview : Form.Review.Field -> String -> Context a -> State
editReview field value model =
    let
        state =
            model.forms
    in
    { state | reviewDrafts = Dict.insert (formKey model AddReview) (Form.Review.edit field value (reviewDraft model)) state.reviewDrafts }


advanceSerial : State -> State
advanceSerial state =
    { state | serial = state.serial + 1 }


openDelete : Snapshot -> State -> State
openDelete snapshot state =
    { state | deletion = Just snapshot }


confirmDelete : String -> State -> State
confirmDelete name state =
    { state | deletion = Maybe.map (\snapshot -> { snapshot | confirmation = name }) state.deletion }


closeDelete : State -> State
closeDelete state =
    { state | deletion = Nothing }


resetPerson : String -> Context a -> State
resetPerson key model =
    let
        state =
            model.forms
    in
    { state | drafts = state.drafts |> Dict.remove (formKey model (UpdatePerson key)) |> Dict.remove (formKey model (DeactivatePerson key)) }


prepareReview : Context a -> State
prepareReview model =
    if get model AddReview "goal" == "" then
        editReview Form.Review.Goal "demo-revenue" model

    else
        model.forms
