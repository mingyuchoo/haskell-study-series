module Main exposing (main)

import Browser
import Browser.Dom
import Dict exposing (Dict)
import Domain exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Http
import Json.Decode as D
import Json.Encode as E
import Task
import Url



-- Navigation and server lifecycle are explicit states, never inferred from the DOM.


type Page
    = Organizations
    | Dashboard
    | Responsibility
    | Authorities
    | Results
    | Reviews
    | Settings


type Remote a
    = Loading
    | Loaded a
    | Failed String


type SaveState
    = Idle
    | Saving String


type Action
    = CreateOrg
    | ImportDemo
    | Rename
    | AddPerson
    | AddGoal
    | Assign String
    | Grant String
    | Report String
    | Strategy String
    | AddReview
    | Activate String
    | Evaluate String
    | DeleteOrg


type alias Flags =
    { seed : String, today : String, deadline : String }


type alias Snapshot =
    { id : String, name : String, version : Int, confirmation : String }


type alias Model =
    { page : Page, org : Maybe String, organizations : Remote (List Summary), workspace : Remote Workspace, drafts : Dict String (Dict String String), request : Int, saving : SaveState, fresh : Bool, notice : String, error : Bool, deletion : Maybe Snapshot, guideOpen : Bool, flags : Flags, serial : Int, goalSerial : Dict String Int, expandedGoal : Maybe String, syncing : Bool }


type Msg
    = Navigate Page (Maybe String)
    | Refresh
    | GotOrganizations Int (Result Http.Error (List Summary))
    | GotWorkspace Int (Result Http.Error Workspace)
    | Edit Action String String
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
    refresh { page = Organizations, org = Nothing, organizations = Loading, workspace = Loading, drafts = Dict.empty, request = 0, saving = Idle, fresh = False, notice = "", error = False, deletion = Nothing, guideOpen = True, flags = flags, serial = 0, goalSerial = Dict.empty, expandedGoal = Nothing, syncing = True }


orgPath : String -> String -> String
orgPath org tail =
    "/api/organizations/"
        ++ Url.percentEncode org
        ++ (if tail == "" then
                ""

            else
                "/" ++ tail
           )



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
            ( next, Http.get { url = "/api/organizations", expect = Http.expectJson (GotOrganizations token) (D.list summaryDecoder) } )

        Just org ->
            ( next, Http.get { url = orgPath org "dashboard", expect = Http.expectJson (GotWorkspace token) workspaceDecoder } )


busy : Model -> Bool
busy model =
    model.saving /= Idle


errorText : Http.Error -> String
errorText err =
    case err of
        Http.BadUrl _ ->
            "요청 주소를 확인할 수 없습니다."

        Http.Timeout ->
            "서버 응답 시간이 초과되었습니다. 입력 내용은 보존됩니다."

        Http.NetworkError ->
            "서버에 연결할 수 없습니다. 연결을 확인하고 다시 시도하세요."

        Http.BadStatus code ->
            "서버 조회 실패 (" ++ String.fromInt code ++ "). 새로고침해 주세요."

        Http.BadBody _ ->
            "서버 응답 형식이 예상과 다릅니다. 입력 내용은 보존됩니다."



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
                        ( { model | organizations = Failed (errorText err), notice = errorText err, error = True, fresh = False, syncing = False }, Cmd.none )

        GotWorkspace token response ->
            if token /= model.request then
                ( model, Cmd.none )

            else
                case response of
                    Ok workspace ->
                        ( { model | workspace = Loaded workspace, fresh = True, syncing = False }, Cmd.none )

                    Err err ->
                        ( { model | workspace = Failed (errorText err), notice = errorText err, error = True, fresh = False, syncing = False }, Cmd.none )

        Edit action key val ->
            if busy model then
                ( model, Cmd.none )

            else
                let
                    draftKey =
                        formKey model action

                    current =
                        Dict.get draftKey model.drafts |> Maybe.withDefault (draftDefaults model action)
                in
                ( { model | drafts = Dict.insert draftKey (Dict.insert "__version" (String.fromInt (workspaceVersion model)) (Dict.insert key val current)) model.drafts }, Cmd.none )

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
                        ( { model | saving = Saving (actionKey action), notice = "저장 중입니다…", error = False, serial = model.serial + 1 }, send model.request action method path body )

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
                                    , notice = "저장했습니다. 최신 조직 상태와 감사 기록을 확인하세요."
                                    , error = False
                                    , deletion = Nothing
                                }
                        in
                        if action == DeleteOrg then
                            refresh { saved | page = Organizations, org = Nothing, workspace = Loading, organizations = Loading, drafts = Dict.filter (\key _ -> not (String.startsWith (Maybe.withDefault "" model.org ++ "/") key)) model.drafts, notice = "조직을 논리 삭제했습니다. 원본 감사 기록과 다른 조직은 보존됩니다." }

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
                    drafts =
                        if page == Reviews && target == "review-form" && get model AddReview "goal" == "" then
                            Dict.insert (formKey model AddReview) (Dict.insert "goal" "demo-revenue" (Dict.get (formKey model AddReview) model.drafts |> Maybe.withDefault Dict.empty)) model.drafts

                        else
                            model.drafts
                in
                ( { model
                    | page = page
                    , drafts = drafts
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


send : Int -> Action -> String -> String -> E.Value -> Cmd Msg
send token action method path body =
    Http.request
        { method = method
        , headers = []
        , url = path
        , body = Http.jsonBody body
        , timeout = Just 30000
        , tracker = Nothing
        , expect =
            Http.expectStringResponse (Saved token action)
                (\response ->
                    case response of
                        Http.BadUrl_ _ ->
                            Err "잘못된 요청 주소입니다."

                        Http.Timeout_ ->
                            Err "응답 시간이 초과되었습니다. 서버에서 이미 저장됐을 수 있으므로 최신 기록을 확인하세요."

                        Http.NetworkError_ ->
                            Err "연결이 끊겼습니다. 서버에서 이미 저장됐을 수 있으므로 최신 기록을 확인하세요."

                        Http.BadStatus_ metadata content ->
                            Err ((D.decodeString (D.field "error" D.string) content |> Result.withDefault "요청을 처리할 수 없습니다.") ++ " (" ++ String.fromInt metadata.statusCode ++ ")")

                        Http.GoodStatus_ _ _ ->
                            Ok ()
                )
        }


actionKey : Action -> String
actionKey action =
    case action of
        CreateOrg ->
            "organization"

        ImportDemo ->
            "demo"

        Rename ->
            "rename"

        AddPerson ->
            "person"

        AddGoal ->
            "goal"

        Assign key ->
            "owner-" ++ key

        Grant key ->
            "authority-" ++ key

        Report key ->
            "result-" ++ key

        Strategy key ->
            "strategy-" ++ key

        AddReview ->
            "review"

        Activate key ->
            "activate-" ++ key

        Evaluate key ->
            "evaluate-" ++ key

        DeleteOrg ->
            "delete"


formKey : Model -> Action -> String
formKey model action =
    Maybe.withDefault "list" model.org ++ "/" ++ actionKey action


get : Model -> Action -> String -> String
get model action name =
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


draftDefaults : Model -> Action -> Dict String String
draftDefaults model action =
    ([ "name", "role", "description", "metricName", "unit", "metricId", "direction", "baseline", "target", "startsAt", "deadline", "budget", "parent", "owner", "reportedBy", "note", "value", "goal", "learning", "decision", "decisionOwner", "decisionDeadline" ] ++ List.map Tuple.first permissions)
        |> List.map (\key -> ( key, defaultValue model action key ))
        |> Dict.fromList


defaultValue : Model -> Action -> String -> String
defaultValue model action name =
    let
        w =
            case model.workspace of
                Loaded data ->
                    Just data

                _ ->
                    Nothing

        owner key =
            w |> Maybe.andThen (\data -> List.filter (.goal >> .id >> (==) key) data.goals |> List.head |> Maybe.andThen .owner) |> Maybe.withDefault ""
    in
    case action of
        Rename ->
            if name == "name" then
                w |> Maybe.map (.organization >> .name) |> Maybe.withDefault ""

            else
                ""

        AddGoal ->
            case name of
                "baseline" ->
                    "0"

                "target" ->
                    "100"

                "budget" ->
                    "0"

                "metricId" ->
                    "metric-" ++ model.flags.seed ++ "-" ++ String.fromInt (Dict.get (Maybe.withDefault "" model.org) model.goalSerial |> Maybe.withDefault 0)

                "direction" ->
                    "HigherIsBetter"

                "startsAt" ->
                    model.flags.today

                "deadline" ->
                    model.flags.deadline

                _ ->
                    ""

        Assign key ->
            if name == "owner" then
                owner key

            else
                ""

        Report key ->
            if name == "reportedBy" then
                owner key

            else
                ""

        Grant key ->
            let
                authority =
                    w |> Maybe.andThen (\data -> List.filter (.owner >> (==) key) data.authorities |> List.head)
            in
            if name == "budget" then
                authority |> Maybe.map (.budgetLimit >> String.fromFloat) |> Maybe.withDefault "0"

            else if authority |> Maybe.map (\a -> List.member name a.canApprove || (name == "Hiring" && a.canHire) || (name == "Pricing" && a.canChangePrice)) |> Maybe.withDefault False then
                "true"

            else
                "false"

        _ ->
            ""



-- Validate at the command boundary as well as with native form constraints.


payload : Model -> Action -> Result String ( String, String, E.Value )
payload model action =
    let
        val =
            get model action

        str key =
            ( key, E.string (val key) )

        uid prefix =
            E.string (prefix ++ "-" ++ model.flags.seed ++ "-" ++ String.fromInt model.serial)

        num key =
            E.float (String.toFloat (val key) |> Maybe.withDefault 0)

        nullable value_ =
            if value_ == "" then
                E.null

            else
                E.string value_

        day key =
            E.string (val key ++ "T00:00:00Z")

        ps =
            E.list E.string (permissions |> List.filter (\( key, _ ) -> val key == "true") |> List.map Tuple.first)

        path tail =
            orgPath (Maybe.withDefault "" model.org) tail

        post route fields =
            Ok ( "POST", route, E.object fields )

        version =
            get model action "__version" |> String.toInt |> Maybe.withDefault (workspaceVersion model)

        blank keys =
            List.any (\key -> String.trim (val key) == "") keys

        badNumber keys =
            List.any (\key -> String.toFloat (val key) == Nothing) keys

        validate keys nums result =
            if blank keys then
                Err "필수 항목을 모두 입력하세요."

            else if badNumber nums then
                Err "숫자 항목을 올바르게 입력하세요."

            else
                result
    in
    case action of
        CreateOrg ->
            validate [ "name" ] [] (post "/api/organizations" [ ( "id", uid "org" ), str "name" ])

        ImportDemo ->
            post "/api/demo" []

        Rename ->
            validate [ "name" ]
                []
                (if version /= workspaceVersion model then
                    Err "작성 중 조직이 변경되었습니다. 최신 조직 이름을 확인하고 수정 입력을 다시 해 주세요."

                 else
                    Ok ( "PATCH", path "", E.object [ str "name", ( "expectedVersion", E.int version ) ] )
                )

        AddPerson ->
            validate [ "name", "role" ] [] (post (path "people") [ ( "id", uid "person" ), str "name", str "role" ])

        AddGoal ->
            validate [ "description", "metricId", "metricName", "unit", "startsAt", "deadline" ]
                [ "baseline", "target", "budget" ]
                (if val "deadline" < val "startsAt" then
                    Err "마감일은 시작일 이후여야 합니다."

                 else
                    post (path "goals") [ ( "id", uid "goal" ), ( "organization", E.string (Maybe.withDefault "" model.org) ), str "description", ( "metric", E.object [ ( "id", E.string (val "metricId") ), ( "name", E.string (val "metricName") ), str "unit", str "direction" ] ), ( "baseline", num "baseline" ), ( "target", num "target" ), ( "startsAt", day "startsAt" ), ( "deadline", day "deadline" ), ( "parent", nullable (val "parent") ), ( "requiredPermissions", ps ), ( "requiredBudget", num "budget" ) ]
                )

        Assign key ->
            validate [ "owner" ] [] (post (path ("goals/" ++ Url.percentEncode key ++ "/owner")) [ str "owner" ])

        Grant key ->
            validate [] [ "budget" ] (post (path ("people/" ++ Url.percentEncode key ++ "/authority")) [ ( "owner", E.string key ), ( "budgetLimit", num "budget" ), ( "canHire", E.bool False ), ( "canChangePrice", E.bool False ), ( "canApprove", ps ) ])

        Report key ->
            validate [ "reportedBy", "note" ] [ "value" ] (post (path ("goals/" ++ Url.percentEncode key ++ "/results")) [ ( "value", num "value" ), str "reportedBy", str "note", ( "actor", E.string (val "reportedBy") ) ])

        Strategy key ->
            validate [ "note" ] [] (post (path ("goals/" ++ Url.percentEncode key ++ "/strategy")) [ str "note" ])

        Activate key ->
            post (path ("goals/" ++ Url.percentEncode key ++ "/activate")) []

        Evaluate key ->
            post (path "evaluations") [ ( "goal", E.string key ) ]

        AddReview ->
            validate [ "goal", "note" ]
                []
                (if String.trim (val "decision") /= "" && val "decisionOwner" == "" then
                    Err "다음 결정의 담당자를 선택하세요."

                 else
                    post (path "reviews")
                        [ ( "id", uid "review" )
                        , str "goal"
                        , str "note"
                        , ( "learnings"
                          , E.list identity
                                (if String.trim (val "learning") == "" then
                                    []

                                 else
                                    [ E.object [ ( "text", E.string (val "learning") ) ] ]
                                )
                          )
                        , ( "decisions"
                          , E.list identity
                                (if String.trim (val "decision") == "" then
                                    []

                                 else
                                    [ E.object
                                        [ ( "text", E.string (val "decision") )
                                        , ( "owner", E.string (val "decisionOwner") )
                                        , ( "deadline"
                                          , if val "decisionDeadline" == "" then
                                                E.null

                                            else
                                                E.string (val "decisionDeadline" ++ "T23:59:59Z")
                                          )
                                        ]
                                    ]
                                )
                          )
                        ]
                )

        DeleteOrg ->
            case model.deletion of
                Just snapshot ->
                    if snapshot.confirmation == snapshot.name && model.org == Just snapshot.id then
                        Ok ( "DELETE", orgPath snapshot.id "", E.object [ ( "confirmName", E.string snapshot.confirmation ), ( "expectedVersion", E.int snapshot.version ) ] )

                    else
                        Err "조직 이름을 정확히 입력하세요."

                Nothing ->
                    Err "삭제 확인을 먼저 열어 주세요."


pageName : Page -> String
pageName page =
    case page of
        Organizations ->
            "조직 목록"

        Dashboard ->
            "목표"

        Responsibility ->
            "책임"

        Authorities ->
            "권한"

        Results ->
            "결과"

        Reviews ->
            "학습"

        Settings ->
            "조직 설정"


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
                organizationsView model

              else
                workspaceView model
            , footer [] [ text "기록된 권한은 실제 시스템 접근 제어와 연결되지 않습니다. 감사 기록의 행위자는 인증된 신원 증명이 아닙니다." ]
            ]
        ]


panel : String -> List (Html Msg) -> Html Msg
panel title children =
    section [ class "panel" ] (h2 [] [ text title ] :: children)


note : String -> Html Msg
note content =
    p [ class "note" ] [ text content ]


emptyState : String -> String -> Html Msg
emptyState title content =
    section [ class "panel empty" ] [ h2 [] [ text title ], p [] [ text content ] ]


remoteView : Remote a -> (a -> Html Msg) -> Html Msg
remoteView remote render =
    case remote of
        Loading ->
            section [ class "panel", attribute "role" "status" ] [ text "워크스페이스를 불러오는 중…" ]

        Failed message ->
            emptyState "조회하지 못했습니다" message

        Loaded data ->
            render data


organizationsView : Model -> Html Msg
organizationsView model =
    div []
        [ panel "새 조직 등록"
            [ note "각 조직의 구성원, 목표와 학습은 독립적으로 관리됩니다."
            , formView model CreateOrg "조직 등록" [ inputField model CreateOrg "조직 이름" "name" "text" True ]
            , button
                [ class "secondary"
                , disabled
                    (busy model
                        || not model.fresh
                        || (case model.organizations of
                                Loaded items ->
                                    List.any (.organization >> .id >> (==) "demo-northstar-v2") items

                                _ ->
                                    True
                           )
                    )
                , onClick (Submit ImportDemo)
                ]
                [ text "체험용 데모 조직 추가" ]
            ]
        , remoteView model.organizations
            (\items ->
                div []
                    [ div [ class "section-head" ] [ h2 [] [ text "등록된 조직" ], span [ class "tag" ] [ text (String.fromInt (List.length items) ++ "개") ] ]
                    , if List.isEmpty items then
                        emptyState "첫 조직을 시작하세요" "조직 이름을 입력하거나 가상 데이터로 운영 흐름을 체험하세요."

                      else
                        div [ class "grid" ]
                            (List.map
                                (\item ->
                                    section [ class "panel organization-card" ]
                                        [ span [ class "tag" ]
                                            [ text
                                                (if item.demo then
                                                    "가상 데이터 · 데모"

                                                 else
                                                    "내 조직"
                                                )
                                            ]
                                        , h2 [] [ text item.organization.name ]
                                        , p [] [ text ("구성원 " ++ String.fromInt item.peopleCount ++ "명 · 목표 " ++ String.fromInt item.goalCount ++ "개") ]
                                        , small [] [ text ("등록 " ++ String.left 10 item.organization.createdAt) ]
                                        , div [ class "actions" ] [ button [ disabled (busy model), onClick (Navigate Dashboard (Just item.organization.id)) ] [ text "조직 열기 →" ], button [ class "secondary", disabled (busy model), onClick (Navigate Settings (Just item.organization.id)) ] [ text "상세 · 수정 · 삭제" ] ]
                                        ]
                                )
                                items
                            )
                    ]
            )
        ]


workspaceView : Model -> Html Msg
workspaceView model =
    remoteView model.workspace
        (\w ->
            div []
                [ if w.demo && model.page /= Settings then
                    guideView model w

                  else
                    text ""
                , case model.page of
                    Dashboard ->
                        dashboardView model w

                    Responsibility ->
                        responsibilityView model w

                    Authorities ->
                        authorityView model w

                    Results ->
                        resultsView model w

                    Reviews ->
                        reviewsView model w

                    Settings ->
                        settingsView model w

                    Organizations ->
                        text ""
                ]
        )


formView : Model -> Action -> String -> List (Html Msg) -> Html Msg
formView model action label_ children =
    Html.form [ onSubmit (Submit action) ]
        [ fieldset [ disabled (busy model) ]
            (children
                ++ [ button [ type_ "submit", disabled (not model.fresh) ]
                        [ text
                            (if model.saving == Saving (actionKey action) then
                                "저장 중…"

                             else
                                label_
                            )
                        ]
                   ]
            )
        ]


inputField : Model -> Action -> String -> String -> String -> Bool -> Html Msg
inputField model action label_ key kind required_ =
    label [] [ text label_, input [ name key, type_ kind, value (get model action key), onInput (Edit action key), required required_, step "any", autocomplete False ] [] ]


selectField : Model -> Action -> String -> String -> Bool -> List ( String, String ) -> Html Msg
selectField model action label_ key required_ options =
    label [] [ text label_, select [ name key, value (get model action key), onInput (Edit action key), required required_ ] (List.map (\( key_, label__ ) -> option [ value key_, selected (get model action key == key_) ] [ text label__ ]) options) ]


checks : Model -> Action -> Html Msg
checks model action =
    fieldset [ class "permission-fields" ]
        [ legend [] [ text "결정 권한" ]
        , div [ class "checks" ]
            (List.map
                (\( key, label_ ) ->
                    label []
                        [ input
                            [ type_ "checkbox"
                            , checked (get model action key == "true")
                            , onCheck
                                (\checked_ ->
                                    Edit action
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


peopleOptions : Workspace -> List ( String, String )
peopleOptions w =
    ( "", "구성원 선택" ) :: List.map (\p -> ( p.id, p.name ++ " · " ++ p.role )) w.people


goalOptions : Workspace -> List ( String, String )
goalOptions w =
    ( "", "목표 선택" ) :: List.map (\g -> ( g.goal.id, g.goal.description )) w.goals


dashboardView : Model -> Workspace -> Html Msg
dashboardView model w =
    div []
        [ div [ class "metrics" ] (List.map (\( label_, amount, desc ) -> div [ class "metric" ] [ span [] [ text label_ ], strong [] [ text (String.fromInt amount) ], small [] [ text desc ] ]) [ ( "전체 목표", List.length w.goals, "측정 가능한 결과" ), ( "활성 목표", List.length (List.filter .active w.goals), "책임과 권한 검증 완료" ), ( "구조 진단", w.compiler.errors + w.compiler.warnings, "확인이 필요한 항목" ), ( "누적 학습", List.sum (List.map (.learnings >> List.length) w.reviews), "다음 결정의 근거" ) ])
        , div [ class "section-head" ] [ h2 [] [ text "목표 포트폴리오" ], a [ href "#new-goal" ] [ text "+ 목표 만들기" ] ]
        , if List.isEmpty w.goals then
            emptyState "어떤 결과를 만들고 싶나요?" "아래에서 측정 가능한 목표를 정의하고 책임자를 연결하세요."

          else
            div [ class "grid" ] (List.map (goalCard model w) w.goals)
        , details [ class "panel", id "new-goal" ] [ summary [] [ text "+ 목표 만들기" ], goalForm model w ]
        , details [ class "panel" ] [ summary [] [ text ("구성원 추가 · 현재 " ++ String.fromInt (List.length w.people) ++ "명") ], formView model AddPerson "구성원 추가" [ div [ class "fields" ] [ inputField model AddPerson "이름" "name" "text" True, inputField model AddPerson "역할" "role" "text" True ] ] ]
        , diagnosticView w
        ]


goalForm : Model -> Workspace -> Html Msg
goalForm model w =
    formView model
        AddGoal
        "목표 초안 생성"
        [ note "초안 → 책임자 지정 → 권한 확인 → 활성화. 필요한 조건을 갖춘 뒤 실행합니다."
        , inputField model AddGoal "어떤 결과를 만들고 싶나요?" "description" "text" True
        , div [ class "fields" ] [ inputField model AddGoal "KPI 이름" "metricName" "text" True, inputField model AddGoal "단위" "unit" "text" True, inputField model AddGoal "지표 식별자 · 같은 지표는 같은 ID" "metricId" "text" True, selectField model AddGoal "좋은 결과의 방향" "direction" True [ ( "HigherIsBetter", "높을수록 좋음" ), ( "LowerIsBetter", "낮을수록 좋음" ) ], inputField model AddGoal "기준값" "baseline" "number" True, inputField model AddGoal "목표값" "target" "number" True, inputField model AddGoal "시작일 (UTC)" "startsAt" "date" True, inputField model AddGoal "마감일 (UTC)" "deadline" "date" True, inputField model AddGoal "필요 예산 (KRW)" "budget" "number" True, selectField model AddGoal "상위 목표 (선택)" "parent" False (( "", "없음" ) :: List.drop 1 (goalOptions w)) ]
        , checks model AddGoal
        ]


badge : GoalView -> Html Msg
badge g =
    span [ classList [ ( "tag", True ), ( "draft", not g.active ), ( "error", g.evaluation.status == OffTrack ), ( "warn", g.evaluation.status == AtRisk ) ] ]
        [ text
            (if g.active then
                statusName g.evaluation.status

             else
                "초안"
            )
        ]


goalCard : Model -> Workspace -> GoalView -> Html Msg
goalCard model w g =
    article [ class "goal-card", id ("goal-" ++ g.goal.id), tabindex -1 ]
        (goalSummary w g
            ++ [ div [ class "actions" ] [ button [ class "secondary", disabled (busy model), onClick (Guide Results ("goal-" ++ g.goal.id)) ] [ text "결과 보고 · 평가 →" ] ]
               , details [ property "open" (E.bool (model.expandedGoal == Just g.goal.id)) ]
                    [ summary [] [ text "책임 · 권한 · 전략 관리" ]
                    , note g.analysis.possibleCause
                    , formView model (Assign g.goal.id) "책임자 지정" [ selectField model (Assign g.goal.id) "단일 최종 책임자" "owner" True (peopleOptions w) ]
                    , note "책임자 변경 또는 권한 부족 시 초안으로 돌아갑니다. 권한 메뉴에서 결정 권한을 조정하세요."
                    , div [ class "actions" ]
                        [ button [ disabled (busy model || not model.fresh || g.active), onClick (Submit (Activate g.goal.id)) ]
                            [ text
                                (if g.active then
                                    "활성화됨"

                                 else
                                    "목표 활성화"
                                )
                            ]
                        ]
                    , formView model (Strategy g.goal.id) "전략 변경 기록" [ inputField model (Strategy g.goal.id) "새로운 전략과 변경 이유" "note" "text" True ]
                    , div [] (List.map (\( at, message ) -> note (String.left 10 at ++ " · " ++ message)) g.strategies)
                    ]
               ]
        )


resultsView : Model -> Workspace -> Html Msg
resultsView model w =
    div []
        [ div [ class "section-head" ] [ h2 [] [ text "목표별 결과와 평가" ] ]
        , note "실측값을 보고하고 현재 성과를 평가하세요. 결과 이력은 다음 학습의 근거가 됩니다."
        , if List.isEmpty w.goals then
            emptyState "아직 측정할 목표가 없습니다" "목표 메뉴에서 목표를 만든 뒤 결과를 기록하세요."

          else
            div [ class "grid" ] (List.map (resultCard model w) w.goals)
        ]


resultCard : Model -> Workspace -> GoalView -> Html Msg
resultCard model w g =
    article [ class "goal-card", id ("goal-" ++ g.goal.id), tabindex -1 ]
        (goalSummary w g
            ++ [ note g.analysis.possibleCause
               , h3 [ class "form-heading" ] [ text "결과 보고" ]
               , formView model (Report g.goal.id) "결과 보고" [ div [ class "fields" ] [ inputField model (Report g.goal.id) "실측값" "value" "number" True, selectField model (Report g.goal.id) "보고자" "reportedBy" True (peopleOptions w) ], inputField model (Report g.goal.id) "결과 설명" "note" "text" True ]
               , if List.isEmpty g.results then
                    note "아직 결과가 없습니다."

                 else
                    div [ class "table-wrap" ] [ h3 [ class "form-heading" ] [ text "결과 추이 · 최근 순" ], table [] [ thead [] [ tr [] [ th [] [ text "기록 시각" ], th [] [ text "측정값" ], th [] [ text "설명" ] ] ], tbody [] (List.map (\r -> tr [] [ td [] [ text r.reportedAt ], td [] [ text (formatNumber r.value) ], td [] [ text r.note ] ]) g.results) ] ]
               , div [ class "actions" ]
                    [ button [ class "secondary", disabled (busy model || not model.fresh), onClick (Submit (Evaluate g.goal.id)) ] [ text "평가 기록" ]
                    , button [ class "secondary", disabled (busy model), onClick (Guide Dashboard ("goal-" ++ g.goal.id)) ] [ text "목표 관리 →" ]
                    ]
               ]
        )


goalSummary : Workspace -> GoalView -> List (Html Msg)
goalSummary w g =
    [ badge g
    , h2 [] [ text g.goal.description ]
    , small []
        [ text
            (g.goal.metric.name
                ++ " · "
                ++ (if g.goal.metric.direction == "HigherIsBetter" then
                        "↑ 증가"

                    else
                        "↓ 감소"
                   )
                ++ " 목표"
            )
        ]
    , div [ class "goal-values" ] [ strong [] [ text (g.evaluation.latestValue |> Maybe.map formatNumber |> Maybe.withDefault "—") ], span [ class "muted" ] [ text ("/ " ++ formatNumber g.goal.target ++ " " ++ g.goal.metric.unit) ] ]
    , progress [ Html.Attributes.max "1", value (String.fromFloat (clamp 0 1 g.evaluation.progress)), attribute "aria-label" "목표 달성률" ] []
    , small [] [ text (String.fromInt (round (g.evaluation.progress * 100)) ++ "% 달성 · 기준 " ++ formatNumber g.goal.baseline) ]
    , div [ class "meta" ] [ span [] [ text (g.owner |> Maybe.map (personName w) |> Maybe.withDefault "책임자 미지정") ], span [] [ text (String.left 10 g.goal.deadline ++ " 마감") ] ]
    ]


diagnosticView : Workspace -> Html Msg
diagnosticView w =
    panel "조직 구조 검사"
        [ div [ class "section-head" ] [ text "다음 행동을 위한 피드백", span [ class "tag warn" ] [ text (String.fromInt w.compiler.errors ++ " 오류 · " ++ String.fromInt w.compiler.warnings ++ " 경고") ] ]
        , if List.isEmpty w.compiler.diagnostics then
            note "구조 검사를 통과했습니다. 결과를 보고하고 학습을 이어가세요."

          else
            div [] (List.map (\d -> div [ classList [ ( "diagnostic", True ), ( "error", d.severity == "Error" ) ] ] [ code [] [ text d.code ], strong [] [ text d.message ], p [] [ text d.subject ], div [] (List.map (\line -> p [] [ text line ]) d.details) ]) w.compiler.diagnostics)
        , note "권한 집중도는 권한 종류와 예산 보유를 각각 1점으로 세는 규칙 기반 추정치입니다."
        ]


responsibilityView : Model -> Workspace -> Html Msg
responsibilityView model w =
    div []
        [ panel "누가 어떤 결과를 책임지는가" [ note "각 목표에는 최종 책임자가 한 명 있습니다.", div [ class "table-wrap" ] [ table [] [ thead [] [ tr [] (List.map (\title -> th [] [ text title ]) [ "결과 / KPI", "최종 책임자", "목표값", "필요 권한 / 통제율", "상태" ]) ], tbody [] (List.map (\g -> tr [ id ("owner-" ++ g.goal.id), tabindex -1 ] [ td [] [ strong [] [ text g.goal.description ], text g.goal.metric.name ], td [] [ formView model (Assign g.goal.id) "책임자 지정" [ selectField model (Assign g.goal.id) "책임자" "owner" True (peopleOptions w) ] ], td [] [ text (formatNumber g.goal.target ++ " " ++ g.goal.metric.unit) ], td [] [ text (String.join " · " (List.map permissionName g.goal.requiredPermissions)), note ("예산 " ++ formatNumber g.goal.requiredBudget ++ "원 · " ++ String.fromInt (round (g.analysis.coverage * 100)) ++ "% 통제") ], td [] [ badge g ] ]) w.goals) ] ] ]
        , section [ class "panel", id "responsibility-graph", tabindex -1 ]
            [ h2 [] [ text "책임 관계 그래프" ]
            , note "사람 → 목표 → 지표. 목표 간 의존 관계와 자원 통제를 연결합니다."
            , if List.isEmpty w.edges then
                note "책임자와 목표를 연결하면 그래프가 만들어집니다."

              else
                div [] (List.map (\edge -> div [ class "graph-edge" ] [ span [] [ text (nodeName w edge.from) ], b [] [ text ("─ " ++ edge.kind ++ " →") ], span [] [ text (nodeName w edge.to) ] ]) w.edges)
            ]
        , diagnosticView w
        ]


nodeName : Workspace -> Node -> String
nodeName w node =
    case node.tag of
        "PersonNode" ->
            personName w node.contents

        "GoalNode" ->
            goalName w node.contents

        _ ->
            node.contents


authorityView : Model -> Workspace -> Html Msg
authorityView model w =
    div []
        [ panel "책임을 실행할 수 있는 권한" [ note "권한을 줄여 활성 목표의 요건이 깨지면 해당 목표는 자동으로 초안으로 돌아갑니다.", note "집중도 = 보유 권한 종류 수 + 예산 보유 1점 / 조직 전체 점수. 실제 의사결정 빈도나 권력의 측정값은 아닙니다." ]
        , if List.isEmpty w.people then
            emptyState "구성원을 먼저 추가하세요" "목표 메뉴에서 구성원을 추가한 뒤 권한을 부여할 수 있습니다."

          else
            div [ class "grid" ] (List.map (\person -> section [ class "panel", id ("authority-" ++ person.id), tabindex -1 ] [ span [ class "tag" ] [ text ("권한 비중 " ++ String.fromInt (round (100 * (Dict.get person.id w.decisionShare |> Maybe.withDefault 0))) ++ "%") ], h2 [ class "form-heading" ] [ text person.name ], p [ class "muted" ] [ text person.role ], formView model (Grant person.id) "권한 저장" [ inputField model (Grant person.id) "집행 가능한 예산 한도 (KRW)" "budget" "number" True, checks model (Grant person.id) ], note ("담당 목표 " ++ String.fromInt (List.length (List.filter (.owner >> (==) (Just person.id)) w.goals)) ++ "개") ]) w.people)
        , diagnosticView w
        ]


reviewsView : Model -> Workspace -> Html Msg
reviewsView model w =
    div []
        [ section [ class "panel", id "review-form", tabindex -1 ] [ h2 [] [ text "회고와 다음 결정 기록" ], formView model AddReview "회고 기록" [ selectField model AddReview "회고할 목표" "goal" True (goalOptions w), inputField model AddReview "회고 요약" "note" "text" True, label [] [ text "새롭게 배운 점 (선택)", textarea [ value (get model AddReview "learning"), onInput (Edit AddReview "learning") ] [] ], inputField model AddReview "다음 결정 (선택)" "decision" "text" False, div [ class "fields" ] [ selectField model AddReview "결정 담당자" "decisionOwner" False (peopleOptions w), inputField model AddReview "결정 기한 (UTC, 선택)" "decisionDeadline" "date" False ], note "현재 최신 결과와 평가가 함께 보존됩니다. 결정과 학습이 모두 없으면 구조 검사가 경고합니다." ] ]
        , div [ class "grid" ]
            (List.map
                (\r ->
                    section [ class "panel" ]
                        [ span [ class "tag" ] [ text (String.left 10 r.heldAt ++ " · " ++ statusName r.evaluation.status) ]
                        , h2 [ class "form-heading" ] [ text (goalName w r.goal) ]
                        , p [] [ text r.note ]
                        , h3 [] [ text "학습" ]
                        , if List.isEmpty r.learnings then
                            note "기록된 학습 없음"

                          else
                            div [] (List.map (\learning -> p [] [ text learning ]) r.learnings)
                        , h3 [] [ text "다음 결정" ]
                        , if List.isEmpty r.decisions then
                            note "기록된 결정 없음"

                          else
                            div [] (List.map (\d -> p [] [ text d.text, br [] [], small [] [ text (personName w d.owner ++ " · " ++ (d.deadline |> Maybe.map (String.left 10) |> Maybe.withDefault "기한 미정")) ] ]) r.decisions)
                        , div [] (w.reviewWarnings |> List.filter (.id >> (==) r.id) |> List.concatMap .warnings |> List.map (\warning -> p [ class "tag warn" ] [ text warning ]))
                        ]
                )
                w.reviews
            )
        , section [ class "panel", id "audit-history", tabindex -1 ]
            [ h2 [] [ text "조직의 의사결정 기록" ]
            , note "서버가 시각과 순번을 부여합니다. 행위자는 요청의 기록 주체이며 인증된 신원 증명이 아닙니다."
            , if List.isEmpty w.events then
                note "아직 기록이 없습니다."

              else
                div [] (List.map (\event -> div [ class "event" ] [ small [] [ text ("#" ++ String.fromInt event.seq ++ " · " ++ event.at), br [] [], text (event.actor |> Maybe.map (personName w) |> Maybe.withDefault "로컬 운영자 (미인증)") ], p [] [ text event.description ] ]) w.events)
            ]
        ]


settingsView : Model -> Workspace -> Html Msg
settingsView model w =
    div []
        [ panel w.organization.name [ dl [ class "organization-meta" ] [ dt [] [ text "조직 ID" ], dd [] [ text w.organization.id ], dt [] [ text "등록일" ], dd [] [ text (String.left 10 w.organization.createdAt) ], dt [] [ text "구성원" ], dd [] [ text (String.fromInt (List.length w.people) ++ "명") ], dt [] [ text "목표" ], dd [] [ text (String.fromInt (List.length w.goals) ++ "개") ] ], button [ disabled (busy model), onClick (Navigate Dashboard model.org) ] [ text "목표 →" ] ]
        , panel "조직 이름 수정" [ formView model Rename "이름 저장" [ inputField model Rename "조직 이름" "name" "text" True, note "구성원과 목표, 기존 기록을 유지합니다. 다른 변경과 충돌하면 최신 상태를 확인한 뒤 다시 저장하세요." ] ]
        , section [ class "panel danger-zone" ]
            [ h2 [] [ text "조직 삭제" ]
            , p [] [ text "구성원, 목표, 책임, 권한, 결과, 평가, 회고와 전략이 현재 워크스페이스에서 제거됩니다." ]
            , note "논리 삭제입니다. 원본 감사 이벤트는 파일·DB에 보존되며 완전히 지워지지 않습니다. 다른 조직은 삭제되지 않습니다."
            , case model.deletion of
                Nothing ->
                    button [ class "danger-outline", disabled (busy model || not model.fresh), onClick OpenDelete ] [ text "삭제 확인 열기…" ]

                Just snapshot ->
                    Html.form
                        [ onSubmit (Submit DeleteOrg)
                        , Html.Events.preventDefaultOn "keydown"
                            (D.field "key" D.string
                                |> D.map
                                    (\key ->
                                        if key == "Escape" then
                                            ( CloseDelete, True )

                                        else
                                            ( NoOp, False )
                                    )
                            )
                        , class "delete-confirmation"
                        , attribute "aria-labelledby" "delete-title"
                        ]
                        [ h3 [ id "delete-title" ] [ text (snapshot.name ++ " 조직을 삭제할까요?") ]
                        , fieldset [ disabled (busy model) ]
                            [ label [] [ text "확인하려면 조직 이름을 정확히 입력하세요", input [ id "delete-confirm", value snapshot.confirmation, onInput ConfirmDelete, autocomplete False, required True ] [] ]
                            , div [ class "actions" ]
                                [ button [ type_ "button", class "secondary", onClick CloseDelete ] [ text "취소" ]
                                , button [ type_ "submit", class "danger", disabled (snapshot.confirmation /= snapshot.name || not model.fresh) ]
                                    [ text
                                        (if busy model then
                                            "삭제 중…"

                                         else
                                            "조직 삭제"
                                        )
                                    ]
                                ]
                            ]
                        ]
            ]
        ]


type alias GuideStep =
    { done : Bool, title : String, instruction : String, page : Page, target : String }


guideView : Model -> Workspace -> Html Msg
guideView model w =
    let
        goal key =
            List.filter (.goal >> .id >> (==) ("demo-" ++ key)) w.goals |> List.head

        active key =
            goal key |> Maybe.map .active |> Maybe.withDefault False

        assigned =
            goal "partners" |> Maybe.andThen .owner |> (/=) Nothing

        ready =
            goal "launch" |> Maybe.map (.analysis >> .coverage >> (==) 1) |> Maybe.withDefault False

        achieved =
            goal "revenue" |> Maybe.map (.evaluation >> .status >> (==) Achieved) |> Maybe.withDefault False

        evaluated =
            List.any (\e -> e.evaluatedGoal == Just "demo-revenue" && e.evaluatedStatus == Just Achieved) w.events

        reviewed =
            List.any (\r -> r.goal == "demo-revenue" && r.evaluation.status == Achieved && not (List.isEmpty r.learnings) && List.any (\d -> d.owner /= "" && d.deadline /= Nothing) r.decisions) w.reviews

        steps =
            [ GuideStep (active "partners")
                "01 · 빈 책임 자리 채우기"
                "파트너십 목표에 계약 권한을 가진 한유진을 최종 책임자로 지정하고 활성화하세요."
                (if assigned then
                    Dashboard

                 else
                    Responsibility
                )
                (if assigned then
                    "goal-demo-partners"

                 else
                    "owner-demo-partners"
                )
            , GuideStep (active "launch")
                "02 · 책임에 맞는 권한 주기"
                "이지원에게 채용 권한과 예산 30,000,000원을 부여하세요. 제품 출시 권한을 유지하고 신제품 출시 목표를 활성화하세요."
                (if ready then
                    Dashboard

                 else
                    Authorities
                )
                (if ready then
                    "goal-demo-launch"

                 else
                    "authority-demo-product"
                )
            , GuideStep (achieved && evaluated) "03 · 결과에서 평가까지" "매출 실측값 50 (단위: 억원)과 보고자, 설명을 보고한 뒤 평가 기록을 누르세요." Results "goal-demo-revenue"
            , GuideStep reviewed "04 · 배움을 다음 결정으로" "매출 목표의 학습과 다음 결정, 담당자, 미래 기한을 기록하세요. 달성 결과와 평가가 함께 보존됩니다." Reviews "review-form"
            ]

        count =
            List.length (List.filter .done steps)
    in
    section [ class "demo-guide" ]
        [ div [ class "demo-heading" ] [ div [] [ span [ class "tag" ] [ text "DEMO · 가상 데이터" ], h2 [] [ text "조직의 운영 흐름, 네 단계로 체험하세요" ], p [] [ text "6명 · 7개 목표 · 5가지 성과 상태. 실제 저장 상태로 진행률을 계산합니다." ] ], span [ class "guide-count" ] [ text (String.fromInt count ++ " / 4 완료") ] ]
        , button
            [ class "guide-toggle secondary"
            , onClick ToggleGuide
            , attribute "aria-expanded"
                (if model.guideOpen then
                    "true"

                 else
                    "false"
                )
            ]
            [ text
                (if model.guideOpen then
                    "체험 가이드 접기"

                 else
                    "체험 가이드 열기"
                )
            ]
        , if model.guideOpen then
            div []
                [ div [ class "guide-steps" ]
                    (List.map
                        (\step_ ->
                            article [ classList [ ( "guide-step", True ), ( "complete", step_.done ) ] ]
                                [ span [ class "step-state" ]
                                    [ text
                                        (if step_.done then
                                            "✓ 완료"

                                         else
                                            "○ 체험 대기"
                                        )
                                    ]
                                , h3 [] [ text step_.title ]
                                , p [] [ text step_.instruction ]
                                , button [ class "secondary", disabled (busy model), onClick (Guide step_.page step_.target) ]
                                    [ text
                                        (if step_.done then
                                            "다시 살펴보기 →"

                                         else
                                            "이 단계 진행 →"
                                        )
                                    ]
                                ]
                        )
                        steps
                    )
                , note "전사 성장 지수는 하위 목표의 자동 합계가 아닌 별도 보고 KPI입니다. 초기 진단과 결과 샘플은 의도한 가상 체험 사례입니다. 감사 시각은 실제 가져온 시각입니다."
                , div [ class "actions" ] [ button [ class "secondary", disabled (busy model), onClick (Guide Responsibility "responsibility-graph") ] [ text "관계 그래프 →" ], button [ class "secondary", disabled (busy model), onClick (Guide Reviews "audit-history") ] [ text "감사 기록 →" ] ]
                ]

          else
            text ""
        ]


formatNumber : Float -> String
formatNumber number =
    let
        parts =
            String.fromFloat (toFloat (round (abs number * 100)) / 100) |> String.split "."

        group reversed =
            if String.length reversed <= 3 then
                reversed

            else
                String.left 3 reversed ++ "," ++ group (String.dropLeft 3 reversed)

        whole =
            List.head parts |> Maybe.withDefault "0" |> String.reverse |> group |> String.reverse

        fraction =
            case List.drop 1 parts |> List.head of
                Just digits ->
                    "." ++ digits

                Nothing ->
                    ""
    in
    (if number < 0 then
        "-"

     else
        ""
    )
        ++ whole
        ++ fraction
