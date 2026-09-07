module App.Agents exposing (Design, State, changed, clearDraft, conflicted, current, edit, init, rebase, receive, remove, saved)

import Dict exposing (Dict)
import Domain.Agent as Agent exposing (Change, Role, Snapshot)


{-| 조직별 설계안 초안. version은 입력을 시작한 시점의 조직 버전이다.
-}
type alias Design =
    { version : Int, agents : List Role }


type alias State =
    { snapshots : Dict String Snapshot, drafts : Dict String Design, errors : Dict String String, loading : Bool }


init : State
init =
    { snapshots = Dict.empty, drafts = Dict.empty, errors = Dict.empty, loading = False }


saved : String -> State -> Maybe Snapshot
saved org state =
    Dict.get org state.snapshots


current : String -> State -> Maybe Design
current org state =
    case Dict.get org state.drafts of
        Just draft ->
            Just draft

        Nothing ->
            saved org state |> Maybe.map (\snapshot -> { version = snapshot.version, agents = snapshot.agents })


edit : String -> Change -> State -> State
edit org change state =
    case current org state of
        Nothing ->
            state

        Just design ->
            { state | drafts = Dict.insert org { design | agents = Agent.apply change design.agents } state.drafts }


receive : String -> Result String Snapshot -> State -> State
receive org result state =
    case result of
        Ok snapshot ->
            { state | snapshots = Dict.insert org snapshot state.snapshots, errors = Dict.remove org state.errors, loading = False }

        Err error ->
            { state | errors = Dict.insert org error state.errors, loading = False }


clearDraft : String -> State -> State
clearDraft org state =
    { state | drafts = Dict.remove org state.drafts }


rebase : String -> State -> State
rebase org state =
    case ( Dict.get org state.drafts, saved org state ) of
        ( Just draft, Just latest ) ->
            { state | drafts = Dict.insert org { draft | version = latest.version } state.drafts }

        _ ->
            state


changed : String -> State -> Bool
changed org state =
    Maybe.map .agents (current org state) /= Maybe.map .agents (saved org state)


conflicted : String -> State -> Bool
conflicted org state =
    case ( Dict.get org state.drafts, saved org state ) of
        ( Just draft, Just latest ) ->
            draft.version /= latest.version

        _ ->
            False


remove : String -> State -> State
remove org state =
    { state | snapshots = Dict.remove org state.snapshots, drafts = Dict.remove org state.drafts, errors = Dict.remove org state.errors }
