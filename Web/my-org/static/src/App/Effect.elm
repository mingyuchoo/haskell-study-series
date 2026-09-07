module App.Effect exposing (Effect(..))

import Form.Action exposing (Action)
import Json.Encode as E


type Effect
    = LoadOrganizations Int
    | LoadWorkspace Int String
    | SaveCommand Int Action String String E.Value
    | FocusElement String
