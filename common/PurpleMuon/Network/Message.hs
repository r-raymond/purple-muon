{-|
Module      : PurpleMuon.Network.Message
Description : Functions that evolve around (dis)assembly of network messages
Copyright   : (c) Robin Raymond, 2016
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}
module PurpleMuon.Network.Message
    ( prepend
    , strip
    , nextMC
    , diffMC
    , moveMC
    ) where

import Protolude

import Data.ByteString as DBS

import PurpleMuon.Network.Types as PNT

-- | Prepend a message with an uuid.
prepend :: PNT.UUID -> PNT.NakedMessage -> PNT.RawMessage
prepend uuid m = PNT.RawMessage ((PNT.unUUID uuid) <> (PNT.unNakedMessage m))

-- | strip an uuid off of a message.
-- Returns Just (stripped message) if the message contains the correct uuid
-- and Nothing otherwise
strip :: PNT.UUID -> PNT.RawMessage -> Maybe PNT.NakedMessage
strip uuid m = if (u == h) then (Just $ PNT.NakedMessage t) else Nothing
  where
    u = (PNT.unUUID uuid)
    l = DBS.length u
    (h, t) = DBS.splitAt l (PNT.unRawMessage m)

-- | Calculate the next message count
nextMC :: PNT.MessageCount -> PNT.MessageCount
nextMC (PNT.MessageCount c) = PNT.MessageCount (c + 1) -- note that Word32 wraps

-- | Calculate the offset of two message types.
-- Guarantees that
--
-- @
-- moveMC a (diffMC a b) = b
-- @
--
-- for all MessageCounts a and b.
diffMC :: PNT.MessageCount -> PNT.MessageCount -> PNT.Offset
diffMC (PNT.MessageCount a) (PNT.MessageCount b) = PNT.Offset $
    if (abs (ai - bi)) < threshold
        then bi - ai
        else bi - ai  + (signum (ai - bi)) * (maxi + 1)
  where
    ai = fromIntegral a
    bi = fromIntegral b
    maxi = fromIntegral (maxBound :: Word32)
    threshold = maxi `div` 2

-- | Move a MessageCount by a given offset
moveMC :: PNT.MessageCount -> PNT.Offset -> PNT.MessageCount
moveMC (PNT.MessageCount m) (PNT.Offset o) = PNT.MessageCount $
    fromIntegral ((fromIntegral m) + o)
