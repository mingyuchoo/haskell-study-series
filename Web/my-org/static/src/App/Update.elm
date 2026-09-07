module App.Update exposing (Model, Msg(..), get, init, payload, update)

import App.Config exposing (Flags)
import App.Discovery as DiscoveryState
import App.Drafts as Drafts
import App.Effect exposing (Effect(..))
import App.Model
import App.PageState as PageState
import App.Session as Session
import Dict
import Domain exposing (..)
import Domain.Discovery as Discovery
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
    | GotDiscovery Int String (Result String Discovery.Snapshot)
    | EditDiscovery Discovery.Change
    | AddObservation
    | AddWorkflow
    | SubmitDiscovery
    | SavedDiscovery Int String (Result String ())
    | ResetDiscovery
    | RebaseDiscovery
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
    ( { model | session = session, forms = { forms | deletion = Nothing }, discovery = setDiscoveryLoading (session.org /= Nothing) model.discovery }
    , effects ++ (session.org |> Maybe.map (\org -> [ LoadDiscovery session.request org ]) |> Maybe.withDefault [])
    )


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
        GotDiscovery token org response ->
            if token /= model.session.request || Just org /= model.session.org then
                ( model, [] )

            else
                ( { model | discovery = DiscoveryState.receive org response model.discovery }, [] )

        EditDiscovery change ->
            if busy model || model.discovery.loading || (model.session.org |> Maybe.map (\org -> Dict.member org model.discovery.errors) |> Maybe.withDefault False) then
                ( model, [] )

            else
                ( { model | discovery = model.session.org |> Maybe.map (\org -> DiscoveryState.edit org change model.discovery) |> Maybe.withDefault model.discovery }, [] )

        AddObservation ->
            update (EditDiscovery (Discovery.AddObservation ("observation-" ++ model.flags.seed ++ "-" ++ String.fromInt model.forms.serial))) { model | forms = Drafts.advanceSerial model.forms }

        AddWorkflow ->
            update (EditDiscovery (Discovery.AddWorkflow ("workflow-" ++ model.flags.seed ++ "-" ++ String.fromInt model.forms.serial))) { model | forms = Drafts.advanceSerial model.forms }

        ResetDiscovery ->
            if busy model then
                ( model, [] )

            else
                ( { model | discovery = model.session.org |> Maybe.map (\org -> DiscoveryState.clearDraft org model.discovery) |> Maybe.withDefault model.discovery }, [] )

        RebaseDiscovery ->
            if busy model then
                ( model, [] )

            else
                ( { model | discovery = model.session.org |> Maybe.map (\org -> DiscoveryState.rebase org model.discovery) |> Maybe.withDefault model.discovery, notice = "최신 버전에 입력을 다시 적용했습니다. 내용을 검토한 뒤 저장하세요." }, [] )

        SubmitDiscovery ->
            submitDiscovery model

        SavedDiscovery token org response ->
            if token /= model.session.request || Just org /= model.session.org then
                ( model, [] )

            else
                case response of
                    Ok _ ->
                        refresh { model | discovery = DiscoveryState.clearDraft org model.discovery, session = Session.finishSave model.session, notice = "현황을 저장했습니다. 저장된 근거로 에이전트 초안을 다시 확인하세요.", error = False }

                    Err message ->
                        refresh { model | session = Session.finishSave model.session, notice = message ++ " 입력은 보존했습니다. 최신 저장 내용과 비교한 뒤 다시 적용하세요.", error = True }

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
                let
                    metric =
                        case model.session.workspace of
                            Loaded workspace ->
                                workspace.goals |> List.map (.goal >> .metric) |> List.map (\metric_ -> ( metric_.id, metric_ )) |> Dict.fromList |> Dict.get val

                            _ ->
                                Nothing

                    next =
                        if field == Form.Goal.MetricId then
                            case metric of
                                Just selected ->
                                    List.foldl (\( key, content ) current -> { current | forms = Drafts.editGoal key content current }) model [ ( Form.Goal.MetricId, selected.id ), ( Form.Goal.MetricName, selected.name ), ( Form.Goal.Unit, selected.unit ), ( Form.Goal.Direction, selected.direction ) ]

                                Nothing ->
                                    List.foldl (\( key, content ) current -> { current | forms = Drafts.editGoal key content current }) { model | forms = Drafts.advanceSerial model.forms } [ ( Form.Goal.MetricId, "metric-" ++ model.flags.seed ++ "-new-" ++ String.fromInt model.forms.serial ), ( Form.Goal.MetricName, "" ), ( Form.Goal.Unit, "" ), ( Form.Goal.Direction, "HigherIsBetter" ) ]

                        else
                            { model | forms = Drafts.editGoal field val model }
                in
                ( next, [] )

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
                        , discovery = model.session.org |> Maybe.map (\org -> DiscoveryState.remove org next.discovery) |> Maybe.withDefault next.discovery
                        , notice = "조직을 논리 삭제했습니다. 원본 감사 기록과 다른 조직은 보존됩니다."
                    }

            else
                refresh next


setDiscoveryLoading : Bool -> DiscoveryState.State -> DiscoveryState.State
setDiscoveryLoading loading state =
    { state | loading = loading }


submitDiscovery : Model -> ( Model, List Effect )
submitDiscovery model =
    case model.session.org of
        Nothing ->
            ( model, [] )

        Just org ->
            case DiscoveryState.current org model.discovery of
                Nothing ->
                    ( model, [] )

                Just snapshot ->
                    if busy model || model.discovery.loading || not model.session.fresh || Dict.member org model.discovery.errors then
                        ( { model | notice = "최신 현황을 불러온 뒤 저장하세요. 입력은 보존됩니다.", error = True }, [] )

                    else if DiscoveryState.conflicted org model.discovery then
                        ( { model | notice = "입력 중 저장된 조직이 변경되었습니다. 최신 저장 내용과 비교한 뒤 다시 적용하세요.", error = True }, [] )

                    else if not (List.isEmpty (Discovery.problems snapshot.discovery)) then
                        ( { model | notice = String.join " " (Discovery.problems snapshot.discovery), error = True }, [] )

                    else
                        ( { model | session = Session.beginSave "discovery" model.session, notice = "현황 저장 중…", error = False }, [ SaveDiscovery model.session.request org snapshot ] )
