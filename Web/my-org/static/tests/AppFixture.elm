module AppFixture exposing (mapPage, mapSession)

import App.Model exposing (Model)
import App.PageState as PageState
import App.Session as Session


mapSession : (Session.State -> Session.State) -> Model -> Model
mapSession f model =
    { model | session = f model.session }


mapPage : (PageState.State -> PageState.State) -> Model -> Model
mapPage f model =
    { model | pageState = f model.pageState }
