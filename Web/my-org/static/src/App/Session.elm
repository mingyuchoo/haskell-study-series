module App.Session exposing (SaveState(..), State, beginSave, busy, finishSave, init, organizationDeleted, receiveOrganizations, receiveWorkspace, refresh, selectOrganization)

import App.Effect exposing (Effect(..))
import Domain exposing (Summary, Workspace)
import Remote exposing (Remote(..))


type SaveState
    = Idle
    | Saving String


type alias State =
    { org : Maybe String
    , organizations : Remote (List Summary)
    , workspace : Remote Workspace
    , request : Int
    , saving : SaveState
    , fresh : Bool
    , syncing : Bool
    }


init : State
init =
    { org = Nothing, organizations = Loading, workspace = Loading, request = 0, saving = Idle, fresh = False, syncing = True }


busy : State -> Bool
busy state =
    state.saving /= Idle


refresh : State -> ( State, List Effect )
refresh state =
    let
        token =
            state.request + 1

        next =
            { state | request = token, fresh = False, syncing = True }
    in
    ( next
    , [ case state.org of
            Nothing ->
                LoadOrganizations token

            Just org ->
                LoadWorkspace token org
      ]
    )


receiveOrganizations : Result String (List Summary) -> State -> State
receiveOrganizations result state =
    { state | organizations = response result, fresh = succeeded result, syncing = False }


receiveWorkspace : Result String Workspace -> State -> State
receiveWorkspace result state =
    { state | workspace = response result, fresh = succeeded result, syncing = False }


response : Result String a -> Remote a
response result =
    case result of
        Ok value ->
            Loaded value

        Err message ->
            Failed message


succeeded : Result String a -> Bool
succeeded result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False


finishSave : State -> State
finishSave state =
    { state | saving = Idle }


selectOrganization : Maybe String -> State -> State
selectOrganization org state =
    { state | org = org, workspace = Loading }


beginSave : String -> State -> State
beginSave key state =
    { state | saving = Saving key }


organizationDeleted : State -> State
organizationDeleted state =
    { state | org = Nothing, organizations = Loading, workspace = Loading }
