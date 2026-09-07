module App.Effect exposing (Effect(..))

import Domain.Agent
import Domain.Discovery exposing (Snapshot)
import Form.Action exposing (Action)
import Json.Encode as E


type Effect
    = LoadOrganizations Int
    | LoadWorkspace Int String
    | SaveCommand Int Action String String E.Value
    | LoadDiscovery Int String
    | SaveDiscovery Int String Snapshot
    | LoadAgents Int String
    | SaveAgents Int String Int (List Domain.Agent.Role)
    | FocusElement String
