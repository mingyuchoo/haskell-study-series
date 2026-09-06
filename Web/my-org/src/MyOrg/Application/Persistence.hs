-- | Storage port acquired by an adapter. Runtime alone owns state publication.
module MyOrg.Application.Persistence
  ( Persistence (..)
  ) where

import MyOrg.Domain.Event.Types

data Persistence = Persistence
  { initialEvents      :: [StoredEvent]
  , persistEvents      :: [StoredEvent] -> IO ()
  , releasePersistence :: IO ()
  }
