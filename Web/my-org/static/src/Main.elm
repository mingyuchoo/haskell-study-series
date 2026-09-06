module Main exposing (Model, Msg(..), SaveState(..), get, init, main, payload, update)

import Api.Command
import Api.Http as Api
import Browser
import Browser.Dom
import Dict exposing (Dict)
import Domain exposing (..)
import Form.Action exposing (..)
import Form.Defaults
import Form.Goal
import Form.Review
import Form.Snapshot exposing (Snapshot)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Encode as E
import Page exposing (Page(..), pageName)
import Page.Authorities
import Page.Goals
import Page.Learning
import Page.Organizations
import Page.Responsibility
import Page.Results
import Page.Settings
import Remote exposing (Remote(..))
import Task
import Ui.Form
import Ui.Guide



-- Navigation and server lifecycle are explicit states, never inferred from the DOM.


type SaveState
    = Idle
    | Saving String


type alias Flags =
    { seed : String, today : String, deadline : String }


type alias Model =
    { page : Page, org : Maybe String, organizations : Remote (List Summary), workspace : Remote Workspace, drafts : Dict String (Dict String String), goalDrafts : Dict String Form.Goal.Draft, reviewDrafts : Dict String Form.Review.Draft, request : Int, saving : SaveState, fresh : Bool, notice : String, error : Bool, deletion : Maybe Snapshot, guideOpen : Bool, flags : Flags, serial : Int, goalSerial : Dict String Int, expandedGoal : Maybe String, syncing : Bool }


type Msg
    = Navigate Page (Maybe String)
    | Refresh
    | GotOrganizations Int (Result Http.Error (List Summary))
    | GotWorkspace Int (Result Http.Error Workspace)
    | Edit Action String String
    | EditGoal Form.Goal.Field String
    | EditReview Form.Review.Field String
    | Submit Action
    | Saved Int Action (Result String ())
    | OpenDelete
    | ConfirmDelete String
    | CloseDelete
    | ToggleGuide
    | Guide Page String
    | NoOp


main : Program Flags Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = always Sub.none }


init : Flags -> ( Model, Cmd Msg )
init flags =
    refresh { page = Organizations, org = Nothing, organizations = Loading, workspace = Loading, drafts = Dict.empty, goalDrafts = Dict.empty, reviewDrafts = Dict.empty, request = 0, saving = Idle, fresh = False, notice = "", error = False, deletion = Nothing, guideOpen = True, flags = flags, serial = 0, goalSerial = Dict.empty, expandedGoal = Nothing, syncing = True }



-- Each read owns a generation. Responses from a previous organization are ignored.


refresh : Model -> ( Model, Cmd Msg )
refresh model =
    let
        token =
            model.request + 1

        next =
            { model | request = token, fresh = False, deletion = Nothing, syncing = True }
    in
    case model.org of
        Nothing ->
            ( next, Api.organizations (GotOrganizations token) )

        Just org ->
            ( next, Api.workspace org (GotWorkspace token) )


busy : Model -> Bool
busy model =
    model.saving /= Idle



-- All transitions pass through update. Mutations are serialized, while drafts
-- remain scoped to their organization and form across navigation and refreshes.


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate page org ->
            if busy model then
                ( model, Cmd.none )

            else if org == model.org && org /= Nothing then
                ( { model | page = page, deletion = Nothing }, Cmd.none )

            else
                refresh { model | page = page, org = org, workspace = Loading, notice = "", error = False, deletion = Nothing }

        Refresh ->
            if busy model then
                ( model, Cmd.none )

            else
                refresh model

        GotOrganizations token response ->
            if token /= model.request then
                ( model, Cmd.none )

            else
                case response of
                    Ok items ->
                        ( { model | organizations = Loaded items, fresh = True, syncing = False }, Cmd.none )

                    Err err ->
                        ( { model | organizations = Failed (Api.errorText err), notice = Api.errorText err, error = True, fresh = False, syncing = False }, Cmd.none )

        GotWorkspace token response ->
            if token /= model.request then
                ( model, Cmd.none )

            else
                case response of
                    Ok workspace ->
                        ( { model | workspace = Loaded workspace, fresh = True, syncing = False }, Cmd.none )

                    Err err ->
                        ( { model | workspace = Failed (Api.errorText err), notice = Api.errorText err, error = True, fresh = False, syncing = False }, Cmd.none )

        Edit action key val ->
            if busy model then
                ( model, Cmd.none )

            else
                case action of
                    AddGoal ->
                        Form.Goal.fromKey key |> Maybe.map (\field -> update (EditGoal field val) model) |> Maybe.withDefault ( model, Cmd.none )

                    AddReview ->
                        Form.Review.fromKey key |> Maybe.map (\field -> update (EditReview field val) model) |> Maybe.withDefault ( model, Cmd.none )

                    _ ->
                        let
                            draftKey =
                                formKey model action

                            current =
                                Dict.get draftKey model.drafts |> Maybe.withDefault (draftDefaults model action)
                        in
                        ( { model | drafts = Dict.insert draftKey (Dict.insert "__version" (String.fromInt (workspaceVersion model)) (Dict.insert key val current)) model.drafts }, Cmd.none )

        EditGoal field val ->
            if busy model then
                ( model, Cmd.none )

            else
                ( { model | goalDrafts = Dict.insert (formKey model AddGoal) (Form.Goal.edit field val (goalDraft model)) model.goalDrafts }, Cmd.none )

        EditReview field val ->
            if busy model then
                ( model, Cmd.none )

            else
                ( { model | reviewDrafts = Dict.insert (formKey model AddReview) (Form.Review.edit field val (reviewDraft model)) model.reviewDrafts }, Cmd.none )

        Submit action ->
            if busy model then
                ( model, Cmd.none )

            else if not model.fresh then
                ( { model | notice = "최신 상태를 먼저 불러와 주세요. 입력 내용은 보존됩니다.", error = True }, Cmd.none )

            else
                case payload model action of
                    Err message ->
                        ( { model | notice = message, error = True }, Cmd.none )

                    Ok ( method, path, body ) ->
                        ( { model | saving = Saving (actionKey action), notice = "저장 중입니다…", error = False, serial = model.serial + 1 }, Api.send (Saved model.request action) method path body )

        Saved token action response ->
            if token /= model.request then
                ( model, Cmd.none )

            else
                case response of
                    Err message ->
                        refresh { model | saving = Idle, deletion = Nothing, notice = message ++ " 자동 재시도하지 않았습니다. 최신 상태를 확인한 뒤 다시 저장하세요. 입력 내용은 보존됩니다.", error = True }

                    Ok _ ->
                        let
                            saved =
                                { model
                                    | goalSerial =
                                        if action == AddGoal then
                                            Dict.update (Maybe.withDefault "" model.org) (Maybe.withDefault 0 >> (+) 1 >> Just) model.goalSerial

                                        else
                                            model.goalSerial
                                    , saving = Idle
                                    , drafts = Dict.remove (formKey model action) model.drafts
                                    , goalDrafts =
                                        if action == AddGoal then
                                            Dict.remove (formKey model action) model.goalDrafts

                                        else
                                            model.goalDrafts
                                    , reviewDrafts =
                                        if action == AddReview then
                                            Dict.remove (formKey model action) model.reviewDrafts

                                        else
                                            model.reviewDrafts
                                    , notice = "저장했습니다. 최신 조직 상태와 감사 기록을 확인하세요."
                                    , error = False
                                    , deletion = Nothing
                                }
                        in
                        if action == DeleteOrg then
                            refresh { saved | page = Organizations, org = Nothing, workspace = Loading, organizations = Loading, drafts = Dict.filter (\key _ -> not (String.startsWith (Maybe.withDefault "" model.org ++ "/") key)) model.drafts, goalDrafts = Dict.filter (\key _ -> not (String.startsWith (Maybe.withDefault "" model.org ++ "/") key)) saved.goalDrafts, reviewDrafts = Dict.filter (\key _ -> not (String.startsWith (Maybe.withDefault "" model.org ++ "/") key)) saved.reviewDrafts, notice = "조직을 논리 삭제했습니다. 원본 감사 기록과 다른 조직은 보존됩니다." }

                        else
                            refresh saved

        OpenDelete ->
            case model.workspace of
                Loaded w ->
                    if model.fresh && not (busy model) then
                        ( { model | deletion = Just { id = w.organization.id, name = w.organization.name, version = w.version, confirmation = "" } }, Task.attempt (always NoOp) (Browser.Dom.focus "delete-confirm") )

                    else
                        ( { model | notice = "최신 조직 정보를 불러온 뒤 다시 확인하세요.", error = True }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ConfirmDelete name ->
            if busy model then
                ( model, Cmd.none )

            else
                ( { model | deletion = Maybe.map (\snapshot -> { snapshot | confirmation = name }) model.deletion }, Cmd.none )

        CloseDelete ->
            if busy model then
                ( model, Cmd.none )

            else
                ( { model | deletion = Nothing }, Cmd.none )

        ToggleGuide ->
            ( { model | guideOpen = not model.guideOpen }, Cmd.none )

        Guide page target ->
            if busy model then
                ( model, Cmd.none )

            else
                let
                    reviews =
                        if page == Reviews && target == "review-form" && get model AddReview "goal" == "" then
                            Dict.insert (formKey model AddReview) (Form.Review.edit Form.Review.Goal "demo-revenue" (reviewDraft model)) model.reviewDrafts

                        else
                            model.reviewDrafts
                in
                ( { model
                    | page = page
                    , reviewDrafts = reviews
                    , expandedGoal =
                        if String.startsWith "goal-" target then
                            Just (String.dropLeft 5 target)

                        else
                            model.expandedGoal
                  }
                , Task.attempt (always NoOp) (Browser.Dom.focus target)
                )

        NoOp ->
            ( model, Cmd.none )


formKey : Model -> Action -> String
formKey model action =
    Maybe.withDefault "list" model.org ++ "/" ++ actionKey action


get : Model -> Action -> String -> String
get model action name =
    case action of
        AddGoal ->
            Form.Goal.fromKey name |> Maybe.map (Form.Goal.value (goalDraft model)) |> Maybe.withDefault ""

        AddReview ->
            Form.Review.fromKey name |> Maybe.map (Form.Review.value (reviewDraft model)) |> Maybe.withDefault ""

        _ ->
            Dict.get (formKey model action) model.drafts |> Maybe.andThen (Dict.get name) |> Maybe.withDefault (defaultValue model action name)


workspaceVersion : Model -> Int
workspaceVersion model =
    case model.workspace of
        Loaded w ->
            w.version

        _ ->
            0



-- Capture defaults on the first edit so later server updates cannot alter a
-- partially entered form. Rename also keeps the version the user last edited.


view : Model -> Html Msg
view model =
    div []
        [ a [ href "#main-content", class "skip-link" ] [ text "본문으로 이동" ]
        , aside []
            [ a [ class "brand", href "/" ] [ text "◈ ", strong [] [ text "my org" ], span [] [ text "CLARITY → ACTION → LEARNING" ] ]
            , div [ class "workspace" ]
                [ text
                    (case model.workspace of
                        Loaded w ->
                            w.organization.name

                        _ ->
                            "조직 워크스페이스"
                    )
                ]
            , nav [ attribute "aria-label" "주요 화면" ]
                (List.map
                    (\page ->
                        button
                            [ type_ "button"
                            , classList [ ( "selected", model.page == page ) ]
                            , attribute "aria-current"
                                (if model.page == page then
                                    "page"

                                 else
                                    "false"
                                )
                            , disabled (busy model || (page /= Organizations && model.org == Nothing))
                            , onClick
                                (Navigate page
                                    (if page == Organizations then
                                        Nothing

                                     else
                                        model.org
                                    )
                                )
                            ]
                            [ text (pageName page) ]
                    )
                    [ Organizations, Dashboard, Responsibility, Authorities, Results, Reviews ]
                )
            , div [ class "aside-foot" ] [ span [ class "dot" ] [], text "명확한 상태, 예측 가능한 변화", p [] [ text "결과를 정의하고", br [] [], text "함께 배우는 조직." ], small [] [ text "Elm UI · Haskell API" ] ]
            ]
        , main_ [ id "main-content", tabindex -1 ]
            [ header []
                [ div [] [ span [ class "eyebrow" ] [ text "WORKSPACE / MY ORG" ], h1 [] [ text (pageName model.page) ], p [] [ text "목표 → 책임 → 권한 → 결과 → 학습. 다음 행동을 명확하게." ] ]
                , div [ class "header-actions" ]
                    [ if model.org /= Nothing then
                        button [ class "secondary", disabled (busy model), onClick (Navigate Settings model.org) ] [ text "조직 설정" ]

                      else
                        text ""
                    , button [ class "secondary", disabled (busy model), onClick Refresh ] [ text "↻ 새로고침" ]
                    ]
                ]
            , div
                [ id "notice"
                , classList [ ( "error", model.error ) ]
                , attribute "role"
                    (if model.error then
                        "alert"

                     else
                        "status"
                    )
                , attribute "aria-live" "polite"
                ]
                [ text model.notice ]
            , div [ class "sync-state", attribute "role" "status" ]
                [ span [ class "dot" ] []
                , text
                    (if busy model then
                        "저장 중 · 완료 후 다음 작업을 진행하세요"

                     else if model.fresh then
                        "최신 상태 · 입력은 화면을 이동해도 유지됩니다"

                     else if model.syncing then
                        "최신 상태를 확인하고 있습니다…"

                     else
                        "최신 상태 확인 실패 · 새로고침해 주세요"
                    )
                ]
            , if model.page == Organizations then
                Page.Organizations.view { forms = formConfig model, organizations = model.organizations, open = \org -> Navigate Dashboard (Just org), settings = \org -> Navigate Settings (Just org) }

              else
                workspaceView model
            , footer [] [ text "기록된 권한은 실제 시스템 접근 제어와 연결되지 않습니다. 감사 기록의 행위자는 인증된 신원 증명이 아닙니다." ]
            ]
        ]


workspaceView : Model -> Html Msg
workspaceView model =
    Remote.view model.workspace
        (\w ->
            div []
                [ if w.demo && model.page /= Settings then
                    Ui.Guide.view { guideOpen = model.guideOpen, busy = busy model, toggle = ToggleGuide, go = Guide } w

                  else
                    text ""
                , case model.page of
                    Dashboard ->
                        Page.Goals.view { draft = goalDraft model, edit = EditGoal, forms = formConfig model, expandedGoal = model.expandedGoal, results = Guide Results } w

                    Responsibility ->
                        Page.Responsibility.view { forms = formConfig model } w

                    Authorities ->
                        Page.Authorities.view { forms = formConfig model } w

                    Results ->
                        Page.Results.view { forms = formConfig model, goals = Guide Dashboard } w

                    Reviews ->
                        Page.Learning.view { draft = reviewDraft model, edit = EditReview, forms = formConfig model } w

                    Settings ->
                        Page.Settings.view { forms = formConfig model, deletion = model.deletion, goals = Navigate Dashboard model.org, openDelete = OpenDelete, closeDelete = CloseDelete, confirmDelete = ConfirmDelete, noOp = NoOp } w

                    Organizations ->
                        text ""
                ]
        )


formConfig : Model -> Ui.Form.Config Msg
formConfig model =
    { busy = busy model
    , fresh = model.fresh
    , saving =
        case model.saving of
            Idle ->
                Nothing

            Saving key ->
                Just key
    , value = get model
    , edit = Edit
    , submit = Submit
    }


goalDraft : Model -> Form.Goal.Draft
goalDraft model =
    Dict.get (formKey model AddGoal) model.goalDrafts |> Maybe.withDefault (Form.Goal.fromValues (defaultValue model AddGoal))


reviewDraft : Model -> Form.Review.Draft
reviewDraft model =
    Dict.get (formKey model AddReview) model.reviewDrafts |> Maybe.withDefault (Form.Review.fromValues (defaultValue model AddReview))


payload : Model -> Action -> Result String ( String, String, E.Value )
payload model action =
    Api.Command.payload { org = model.org, seed = model.flags.seed, serial = model.serial, version = workspaceVersion model, deletion = model.deletion, value = get model, goal = goalDraft model, review = reviewDraft model } action


defaultContext : Model -> Form.Defaults.Context
defaultContext model =
    { seed = model.flags.seed
    , today = model.flags.today
    , deadline = model.flags.deadline
    , goalIndex = Dict.get (Maybe.withDefault "" model.org) model.goalSerial |> Maybe.withDefault 0
    , workspace =
        case model.workspace of
            Loaded data ->
                Just data

            _ ->
                Nothing
    }


defaultValue : Model -> Action -> String -> String
defaultValue model =
    Form.Defaults.defaultValue (defaultContext model)


draftDefaults : Model -> Action -> Dict String String
draftDefaults model =
    Form.Defaults.draftDefaults (defaultContext model)
