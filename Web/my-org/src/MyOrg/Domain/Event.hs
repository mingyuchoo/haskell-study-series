-- | Compatibility facade for event types, state, queries and transitions.
module MyOrg.Domain.Event
  (module MyOrg.Domain.Event.Types, module MyOrg.Domain.State, module MyOrg.Domain.Queries, module MyOrg.Domain.Validation, module MyOrg.Domain.Reducer, describeEvent) where

import MyOrg.Domain.Event.Types
import MyOrg.Domain.State
import MyOrg.Domain.Queries
import MyOrg.Domain.Validation
import MyOrg.Domain.Reducer
import MyOrg.Presentation.Event (describeEvent)
