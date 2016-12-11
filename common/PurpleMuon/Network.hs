module PurpleMuon.Network
    ( MessageCount(..)
    , RawMessage
    , NakedMessage
    , prepend
    , strip
    , nextMC
    , diffMC
    , moveMC
    , UUID(..)
    , Offset(..)
    , AckField(..)
    ) where

import           Protolude

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
prepend uuid m = RawMessage ((unUUID uuid) <> (unNakedMessage m))

-- | strip an uuid off of a message.
-- Returns Just (stripped message) if the message contains the correct uuid
-- and Nothing otherwise
strip :: UUID -> RawMessage -> Maybe NakedMessage
strip uuid m = if (u == h) then (Just $ NakedMessage t) else Nothing
  where
    u = (unUUID uuid)
    l = DBS.length u
    (h, t) = DBS.splitAt l (unRawMessage m)

-- | A counter to count both the remote and local messages
newtype MessageCount = MessageCount { unMessageCounter :: Word32 }

-- | A field indicating which of the last 32 messages have arrived.
newtype AckField = AckField { unAckField :: Word32 }

-- | Calculate the next message count
nextMC :: MessageCount -> MessageCount
nextMC (MessageCount c) = MessageCount (c + 1) -- note that Word32 wraps

-- | A difference between to Message counts
newtype Offset = Offset { unOffset :: Int }

-- | Calculate the offset of two message types.
-- Guarantees that
-- ```
-- a + (diffMC a b) = b
-- ```
-- for all MessageCounts a and b.
diffMC :: MessageCount -> MessageCount -> Offset
diffMC (MessageCount a) (MessageCount b) = Offset $
    if (abs (ai - bi)) < threshold
        then bi - ai
        else bi - ai  + (signum (ai - bi)) * (maxi + 1)
  where
    ai = fromIntegral a
    bi = fromIntegral b
    maxi = fromIntegral (maxBound :: Word32)
    threshold = maxi `div` 2

-- | Move a MessageCount by a given offset
moveMC :: MessageCount -> Offset -> MessageCount
moveMC (MessageCount m) (Offset o) = MessageCount $
    fromIntegral ((fromIntegral m) + o)
