module App.Discovery exposing (State, changed, clearDraft, conflicted, current, edit, init, rebase, receive, remove, saved)

import Dict exposing (Dict)
import Domain.Discovery as Discovery exposing (Change, Snapshot)


type alias State =
    { documents : Dict String Snapshot, drafts : Dict String Snapshot, errors : Dict String String, loading : Bool }


init : State
init =
    { documents = Dict.empty, drafts = Dict.empty, errors = Dict.empty, loading = False }


saved : String -> State -> Maybe Snapshot
saved org state =
    Dict.get org state.documents


current : String -> State -> Maybe Snapshot
current org state =
    case Dict.get org state.drafts of
        Just draft ->
            Just draft

        Nothing ->
            saved org state


edit : String -> Change -> State -> State
edit org change state =
    case current org state of
        Nothing ->
            state

        Just snapshot ->
            { state | drafts = Dict.insert org { snapshot | discovery = Discovery.apply change snapshot.discovery } state.drafts }


receive : String -> Result String Snapshot -> State -> State
receive org result state =
    case result of
        Ok snapshot ->
            { state | documents = Dict.insert org snapshot state.documents, errors = Dict.remove org state.errors, loading = False }

        Err error ->
            { state | errors = Dict.insert org error state.errors, loading = False }


clearDraft : String -> State -> State
clearDraft org state =
    { state | drafts = Dict.remove org state.drafts }


rebase : String -> State -> State
rebase org state =
    case ( Dict.get org state.drafts, saved org state ) of
        ( Just draft, Just latest ) ->
            let
                doc =
                    draft.discovery
            in
            { state | drafts = Dict.insert org { draft | version = latest.version, discovery = { doc | review = { status = "pending", note = doc.review.note } } } state.drafts }

        _ ->
            state


changed : String -> State -> Bool
changed org state =
    Maybe.map .discovery (current org state) /= Maybe.map .discovery (saved org state)


conflicted : String -> State -> Bool
conflicted org state =
    case ( current org state, saved org state ) of
        ( Just draft, Just latest ) ->
            draft.version /= latest.version

        _ ->
            False


remove : String -> State -> State
remove org state =
    { state | documents = Dict.remove org state.documents, drafts = Dict.remove org state.drafts, errors = Dict.remove org state.errors }
