module PurpleMuon.Network
    ( MessageCount
    ) where

import Protolude

import qualified Data.ByteString as DBS

-- | A binary message that is send over the network
newtype RawMessage = RawMessage { unRawMessage :: ByteString }

-- | A binary message without a uuid header
newtype NakedMessage = NakedMessage { unNakedMessage :: ByteString }

-- | A uuid is a header that the server and client prepends to
-- every message. A message that does not have such a header is
-- discarded immidieately
newtype UUID = UUID { unUUID :: ByteString }

-- | Prepend a message with an uuid.
prepend :: UUID -> NakedMessage -> RawMessage
prepend uuid msg = RawMessage ((unUUID uuid) <> (unNakedMessage msg))

-- | strip an uuid off of a message.
-- Returns Just (stripped message) if the message contains the correct uuid
-- and Nothing otherwise
strip :: UUID -> RawMessage -> Maybe NakedMessage
strip uuid msg = if (u == h) then (Just $ NakedMessage t) else Nothing
  where
    u = (unUUID uuid)
    l = DBS.length u
    (h, t) = DBS.splitAt l (unRawMessage msg)

-- | A counter to count both the remote and local messages
newtype MessageCount = MessageCount { unMessageCounter :: Word32 }

-- | A field indicating which of the last 32 messages have arrived.
newtype AckField = AckField { unAckField :: Word32 }
