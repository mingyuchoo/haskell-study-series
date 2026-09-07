module App.Model exposing (Model, init)

import App.Config exposing (Flags)
import App.Drafts as Drafts
import App.PageState as PageState
import App.Session as Session


type alias Model =
    { session : Session.State, forms : Drafts.State, pageState : PageState.State, flags : Flags, notice : String, error : Bool }


init : Flags -> Model
init flags =
    { session = Session.init, forms = Drafts.init, pageState = PageState.init, flags = flags, notice = "", error = False }
