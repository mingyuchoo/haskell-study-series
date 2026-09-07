module App.Update exposing (Model, Msg(..), get, init, payload, update)

import App.Config exposing (Flags)
import App.Drafts as Drafts
import App.Effect exposing (Effect(..))
import App.Model
import App.PageState as PageState
import App.Session as Session
import Domain exposing (..)
import Form.Action exposing (..)
import Form.Goal
import Form.Review
import Json.Encode as E
import Page exposing (Page(..))
import Remote exposing (Remote(..))
import Ui.Activity as Activity
import Ui.ListView exposing (Mode)
import Ui.ResponsibilityGraph as Graph


type alias Model =
    App.Model.Model


type Msg
    = Navigate Page (Maybe String)
    | SetListMode Page Mode
    | ActivityChange Activity.State
    | OpenReviewActivity String
    | GraphMsg Graph.Msg
    | GraphGo String
    | Refresh
    | GotOrganizations Int (Result String (List Summary))
    | GotWorkspace Int (Result String Workspace)
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
    | SearchPeople String
    | FilterPeople String
    | ResetPerson String
    | OpenPerson String
    | NoOp


init : Flags -> ( Model, List Effect )
init flags =
    refresh (App.Model.init flags)


refresh : Model -> ( Model, List Effect )
refresh model =
    let
        ( session, effects ) =
            Session.refresh model.session

        forms =
            model.forms
    in
    ( { model | session = session, forms = { forms | deletion = Nothing } }, effects )


busy : Model -> Bool
busy model =
    Session.busy model.session


receive : Int -> Result String a -> Session.State -> Model -> ( Model, List Effect )
receive token result session model =
    if token /= model.session.request then
        ( model, [] )

    else
        case result of
            Ok _ ->
                ( { model | session = session }, [] )

            Err message ->
                ( { model | session = session, notice = message, error = True }, [] )


get : Model -> Action -> String -> String
get =
    Drafts.get


payload : Model -> Action -> Result String ( String, String, E.Value )
payload =
    Drafts.payload


update : Msg -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        ActivityChange state ->
            ( { model | pageState = PageState.setActivity state model.pageState }, [] )

        OpenReviewActivity review ->
            if busy model then
                ( model, [] )

            else
                update (Guide ActivityLog "audit-history") model
                    |> Tuple.mapFirst (\next -> { next | pageState = PageState.setActivity { query = "", kind = "", from = "", until = "", review = Just review } next.pageState })

        GraphGo target ->
            if busy model then
                ( model, [] )

            else if String.startsWith "person:" target then
                update (OpenPerson (String.dropLeft 7 target)) { model | pageState = PageState.setPage People model.pageState }

            else if String.startsWith "authority-" target then
                update (Guide Authorities target) model

            else
                update (Guide Responsibility target) model

        GraphMsg graphMsg ->
            ( { model | pageState = PageState.updateGraph graphMsg model.pageState }, [] )

        SetListMode page mode ->
            ( { model | pageState = PageState.setListMode page mode model.pageState }, [] )

        Navigate page org ->
            if busy model then
                ( model, [] )

            else if org == model.session.org && org /= Nothing then
                ( { model | pageState = PageState.setPage page model.pageState, forms = Drafts.closeDelete model.forms }, [] )

            else
                refresh
                    { model
                        | pageState = PageState.navigate page model.pageState
                        , session = Session.selectOrganization org model.session
                        , forms = Drafts.closeDelete model.forms
                        , notice = ""
                        , error = False
                    }

        Refresh ->
            if busy model then
                ( model, [] )

            else
                refresh model

        GotOrganizations token response ->
            receive token response (Session.receiveOrganizations response model.session) model

        GotWorkspace token response ->
            receive token response (Session.receiveWorkspace response model.session) model

        Edit action key val ->
            if busy model then
                ( model, [] )

            else
                case action of
                    AddGoal ->
                        Form.Goal.fromKey key |> Maybe.map (\field -> update (EditGoal field val) model) |> Maybe.withDefault ( model, [] )

                    AddReview ->
                        Form.Review.fromKey key |> Maybe.map (\field -> update (EditReview field val) model) |> Maybe.withDefault ( model, [] )

                    _ ->
                        ( { model | forms = Drafts.edit action key val model }, [] )

        EditGoal field val ->
            if busy model then
                ( model, [] )

            else
                ( { model | forms = Drafts.editGoal field val model }, [] )

        EditReview field val ->
            if busy model then
                ( model, [] )

            else
                ( { model | forms = Drafts.editReview field val model }, [] )

        Submit action ->
            submit action model

        Saved token action response ->
            if token /= model.session.request then
                ( model, [] )

            else
                saved action response model

        OpenDelete ->
            case model.session.workspace of
                Loaded workspace ->
                    if model.session.fresh && not (busy model) then
                        ( { model | forms = Drafts.openDelete { id = workspace.organization.id, name = workspace.organization.name, version = workspace.version, confirmation = "" } model.forms }, [ FocusElement "delete-confirm" ] )

                    else
                        ( { model | notice = "최신 조직 정보를 불러온 뒤 다시 확인하세요.", error = True }, [] )

                _ ->
                    ( model, [] )

        ConfirmDelete name ->
            if busy model then
                ( model, [] )

            else
                ( { model | forms = Drafts.confirmDelete name model.forms }, [] )

        CloseDelete ->
            if busy model then
                ( model, [] )

            else
                ( { model | forms = Drafts.closeDelete model.forms }, [] )

        ToggleGuide ->
            ( { model | pageState = PageState.toggleGuide model.pageState }, [] )

        Guide page target ->
            if busy model then
                ( model, [] )

            else
                ( { model
                    | pageState = PageState.guide page target model.pageState
                    , forms =
                        if page == Reviews && target == "review-form" then
                            Drafts.prepareReview model

                        else
                            model.forms
                  }
                , [ FocusElement target ]
                )

        SearchPeople query ->
            ( { model | pageState = PageState.searchPeople query model.pageState }, [] )

        FilterPeople status ->
            ( { model | pageState = PageState.filterPeople status model.pageState }, [] )

        ResetPerson key ->
            if busy model then
                ( model, [] )

            else
                refresh
                    { model
                        | forms = Drafts.resetPerson key model
                        , notice = "구성원 수정·인계 입력을 초기화하고 최신 정보를 불러옵니다. 확인한 뒤 다시 작성하세요."
                        , error = False
                    }

        OpenPerson key ->
            if busy model then
                ( model, [] )

            else
                ( { model | pageState = PageState.openPerson key model.pageState }, [ FocusElement "person-detail" ] )

        NoOp ->
            ( model, [] )


submit : Action -> Model -> ( Model, List Effect )
submit action model =
    if busy model then
        ( model, [] )

    else if not model.session.fresh then
        ( { model | notice = "최신 상태를 먼저 불러와 주세요. 입력 내용은 보존됩니다.", error = True }, [] )

    else
        case payload model action of
            Err message ->
                ( { model | notice = message, error = True }, [] )

            Ok ( method, path, body ) ->
                ( { model
                    | session = Session.beginSave (actionKey action) model.session
                    , forms = Drafts.advanceSerial model.forms
                    , notice = "저장 중입니다…"
                    , error = False
                  }
                , [ SaveCommand model.session.request action method path body ]
                )


saved : Action -> Result String () -> Model -> ( Model, List Effect )
saved action response model =
    case response of
        Err message ->
            refresh
                { model
                    | session = Session.finishSave model.session
                    , forms = Drafts.closeDelete model.forms
                    , notice = message ++ " 자동 재시도하지 않았습니다. 최신 상태를 확인한 뒤 다시 저장하세요. 입력 내용은 보존됩니다."
                    , error = True
                }

        Ok _ ->
            let
                next =
                    { model
                        | forms = Drafts.saved action model
                        , session = Session.finishSave model.session
                        , notice = "저장했습니다. 최신 조직 상태와 감사 기록을 확인하세요."
                        , error = False
                    }
            in
            if action == DeleteOrg then
                refresh
                    { next
                        | pageState = PageState.setPage Organizations next.pageState
                        , session = Session.organizationDeleted next.session
                        , forms = Drafts.removeOrganization model.session.org next.forms
                        , notice = "조직을 논리 삭제했습니다. 원본 감사 기록과 다른 조직은 보존됩니다."
                    }

            else
                refresh next
