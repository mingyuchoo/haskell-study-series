module App.Effect exposing (Effect(..))

import Domain.Discovery exposing (Snapshot)
import Form.Action exposing (Action)
import Json.Encode as E


type Effect
    = LoadOrganizations Int
    | LoadWorkspace Int String
    | SaveCommand Int Action String String E.Value
    | LoadDiscovery Int String
    | SaveDiscovery Int String Snapshot
    | FocusElement String
