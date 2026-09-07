module App.Model exposing (Model, init)

import App.Agents as Agents
import App.Config exposing (Flags)
import App.Discovery as Discovery
import App.Drafts as Drafts
import App.PageState as PageState
import App.Session as Session


type alias Model =
    { session : Session.State, forms : Drafts.State, pageState : PageState.State, flags : Flags, notice : String, error : Bool, discovery : Discovery.State, agents : Agents.State }


init : Flags -> Model
init flags =
    { session = Session.init, forms = Drafts.init, pageState = PageState.init, flags = flags, notice = "", error = False, discovery = Discovery.init, agents = Agents.init }
