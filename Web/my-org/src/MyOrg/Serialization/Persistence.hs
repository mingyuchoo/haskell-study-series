-- | The persisted audit format. This boundary depends only on stable event
-- codecs, never HTTP response projections or presentation text.
module MyOrg.Serialization.Persistence
  ( encodeStoredEvent
  , decodeStoredEvent
  , encodeStoredEvents
  , decodeStoredEvents
  ) where

import Data.ByteString.Lazy (ByteString)
import MyOrg.Domain.Event.Types (StoredEvent)
import MyOrg.Serialization.Codec (decodeBytes, encodeBytes, listCodec)
import MyOrg.Serialization.Event (storedEventCodec)

encodeStoredEvent :: StoredEvent -> ByteString
encodeStoredEvent = encodeBytes storedEventCodec

decodeStoredEvent :: ByteString -> Either String StoredEvent
decodeStoredEvent = decodeBytes storedEventCodec

encodeStoredEvents :: [StoredEvent] -> ByteString
encodeStoredEvents = encodeBytes (listCodec storedEventCodec)

decodeStoredEvents :: ByteString -> Either String [StoredEvent]
decodeStoredEvents = decodeBytes (listCodec storedEventCodec)
