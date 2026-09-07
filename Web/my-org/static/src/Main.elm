module Main exposing (main)

import Api.Http as Api
import App.Config exposing (Flags)
import App.Drafts exposing (get, goalDraft, reviewDraft)
import App.Effect exposing (Effect(..))
import App.Model exposing (Model)
import App.PageState as PageState
import App.Session as Session exposing (SaveState(..))
import App.Update as Update exposing (Msg(..))
import Browser
import Browser.Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Page exposing (Page(..), pageName)
import Page.Activity
import Page.Authorities
import Page.Goals
import Page.Learning
import Page.Organizations
import Page.People
import Page.Responsibility
import Page.Results
import Page.Settings
import Remote exposing (Remote(..))
import Task
import Ui.Form
import Ui.Guide
import Ui.ListView as ListView exposing (Mode(..))


main : Program Flags Model Msg
main =
    Browser.element { init = Update.init >> runEffects, update = \msg model -> Update.update msg model |> runEffects, view = view, subscriptions = always Sub.none }


runEffects : ( Model, List Effect ) -> ( Model, Cmd Msg )
runEffects ( model, effects ) =
    ( model, Cmd.batch (List.map perform effects) )


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        LoadOrganizations token ->
            Api.organizations (Result.mapError Api.errorText >> GotOrganizations token)

        LoadWorkspace token org ->
            Api.workspace org (Result.mapError Api.errorText >> GotWorkspace token)

        SaveCommand token action method path body ->
            Api.send (Saved token action) method path body

        FocusElement target ->
            Task.attempt (always NoOp) (Browser.Dom.focus target)


busy : Model -> Bool
busy model =
    Session.busy model.session


listMode : Model -> ListView.Mode
listMode model =
    PageState.listMode model.pageState


view : Model -> Html Msg
view model =
    div []
        [ a [ href "#main-content", class "skip-link" ] [ text "본문으로 이동" ]
        , aside []
            [ a [ class "brand", href "/" ] [ text "◈ ", strong [] [ text "my org" ], span [] [ text "CLARITY → ACTION → LEARNING" ] ]
            , div [ class "workspace" ]
                [ text
                    (case model.session.workspace of
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
                            , classList [ ( "selected", model.pageState.page == page ) ]
                            , attribute "aria-current"
                                (if model.pageState.page == page then
                                    "page"

                                 else
                                    "false"
                                )
                            , disabled (busy model || (page /= Organizations && model.session.org == Nothing))
                            , onClick
                                (Navigate page
                                    (if page == Organizations then
                                        Nothing

                                     else
                                        model.session.org
                                    )
                                )
                            ]
                            [ text (pageName page) ]
                    )
                    [ Organizations, People, Dashboard, Responsibility, Authorities, Results, Reviews, ActivityLog ]
                )
            , div [ class "aside-foot" ] [ span [ class "dot" ] [], text "명확한 상태, 예측 가능한 변화", p [] [ text "결과를 정의하고", br [] [], text "함께 배우는 조직." ], small [] [ text "Elm UI · Haskell API" ] ]
            ]
        , main_ [ id "main-content", tabindex -1 ]
            [ header []
                [ div [] [ span [ class "eyebrow" ] [ text "WORKSPACE / MY ORG" ], h1 [] [ text (pageName model.pageState.page) ], p [] [ text "목표 → 책임 → 권한 → 결과 → 학습. 다음 행동을 명확하게." ] ]
                , div [ class "header-actions" ]
                    [ if model.session.org /= Nothing then
                        button [ class "secondary", disabled (busy model), onClick (Navigate Settings model.session.org) ] [ text "조직 설정" ]

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

                     else if model.session.fresh then
                        "최신 상태 · 입력은 화면을 이동해도 유지됩니다"

                     else if model.session.syncing then
                        "최신 상태를 확인하고 있습니다…"

                     else
                        "최신 상태 확인 실패 · 새로고침해 주세요"
                    )
                ]
            , if model.pageState.page /= Settings then
                ListView.controls (listMode model) (SetListMode model.pageState.page)

              else
                text ""
            , if model.pageState.page == Organizations then
                Page.Organizations.viewWith (listMode model) { forms = formConfig model, organizations = model.session.organizations, open = \org -> Navigate Dashboard (Just org), settings = \org -> Navigate Settings (Just org) }

              else
                workspaceView model
            , footer [] [ text "기록된 권한은 실제 시스템 접근 제어와 연결되지 않습니다. 감사 기록의 행위자는 인증된 신원 증명이 아닙니다." ]
            ]
        ]


workspaceView : Model -> Html Msg
workspaceView model =
    Remote.view model.session.workspace
        (\w ->
            div []
                [ if w.demo && model.pageState.page /= Settings then
                    Ui.Guide.view { guideOpen = model.pageState.guideOpen, busy = busy model, toggle = ToggleGuide, go = Guide } w

                  else
                    text ""
                , case model.pageState.page of
                    People ->
                        Page.People.viewWith (listMode model) { forms = formConfig model, query = model.pageState.peopleQuery, status = model.pageState.peopleStatus, selected = model.pageState.selectedPerson, search = SearchPeople, filter = FilterPeople, open = OpenPerson, reset = ResetPerson, goals = Navigate Dashboard model.session.org } w

                    Dashboard ->
                        Page.Goals.viewWith (listMode model) { draft = goalDraft model, edit = EditGoal, forms = formConfig model, expandedGoal = model.pageState.expandedGoal, results = Guide Results } w

                    Responsibility ->
                        Page.Responsibility.viewInteractive (listMode model) model.pageState.graph (Just GraphMsg) (Just GraphGo) { forms = formConfig model } w

                    Authorities ->
                        Page.Authorities.viewWith (listMode model) { forms = formConfig model } w

                    Results ->
                        Page.Results.viewWith (listMode model) { forms = formConfig model, goals = Guide Dashboard } w

                    Reviews ->
                        Page.Learning.viewWithActivity (Just OpenReviewActivity) (listMode model) { draft = reviewDraft model, edit = EditReview, forms = formConfig model } w

                    ActivityLog ->
                        Page.Activity.view (listMode model) model.pageState.activity ActivityChange w

                    Settings ->
                        Page.Settings.view { forms = formConfig model, deletion = model.forms.deletion, goals = Navigate Dashboard model.session.org, openDelete = OpenDelete, closeDelete = CloseDelete, confirmDelete = ConfirmDelete, noOp = NoOp } w

                    Organizations ->
                        text ""
                ]
        )


formConfig : Model -> Ui.Form.Config Msg
formConfig model =
    { busy = busy model
    , fresh = model.session.fresh
    , saving =
        case model.session.saving of
            Idle ->
                Nothing

            Saving key ->
                Just key
    , value = get model
    , edit = Edit
    , submit = Submit
    }
