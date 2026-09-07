module App.PageState exposing (State, filterPeople, guide, init, listMode, navigate, openPerson, searchPeople, setActivity, setListMode, setPage, toggleGuide, updateGraph)

import Dict exposing (Dict)
import Page exposing (Page(..), pageName)
import Ui.Activity
import Ui.ListView exposing (Mode(..))
import Ui.ResponsibilityGraph as Graph


type alias State =
    { activity : Ui.Activity.State
    , graph : Graph.State
    , listModes : Dict String Mode
    , page : Page
    , guideOpen : Bool
    , expandedGoal : Maybe String
    , peopleQuery : String
    , peopleStatus : String
    , selectedPerson : Maybe String
    }


init : State
init =
    { activity = Ui.Activity.init, graph = Graph.init, listModes = Dict.empty, page = Organizations, guideOpen = False, expandedGoal = Nothing, peopleQuery = "", peopleStatus = "active", selectedPerson = Nothing }


navigate : Page -> State -> State
navigate page state =
    { state | activity = Ui.Activity.init, graph = Graph.init, page = page, peopleQuery = "", peopleStatus = "active", selectedPerson = Nothing }


listMode : State -> Mode
listMode state =
    Dict.get (pageName state.page) state.listModes |> Maybe.withDefault Table


setPage : Page -> State -> State
setPage page state =
    { state | page = page }


setActivity : Ui.Activity.State -> State -> State
setActivity activity state =
    { state | activity = activity }


updateGraph : Graph.Msg -> State -> State
updateGraph message state =
    { state | graph = Graph.update message state.graph }


setListMode : Page -> Mode -> State -> State
setListMode page mode state =
    { state | listModes = Dict.insert (pageName page) mode state.listModes }


guide : Page -> String -> State -> State
guide page target state =
    { state
        | page = page
        , activity =
            if page == ActivityLog then
                Ui.Activity.init

            else
                state.activity
        , expandedGoal =
            if String.startsWith "goal-" target then
                Just (String.dropLeft 5 target)

            else
                state.expandedGoal
    }


toggleGuide : State -> State
toggleGuide state =
    { state | guideOpen = not state.guideOpen }


searchPeople : String -> State -> State
searchPeople query state =
    { state | peopleQuery = query }


filterPeople : String -> State -> State
filterPeople status state =
    { state | peopleStatus = status }


openPerson : String -> State -> State
openPerson key state =
    { state | selectedPerson = Just key }
