--  Copyright 2016, 2017 Robin Raymond
--
--  This file is part of Purple Muon
--
--  Purple Muon is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  Purple Muon is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Purple Muon.  If not, see <http://www.gnu.org/licenses/>.

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

import           Protolude

import qualified Data.Binary              as DBI
import qualified Data.ByteString          as DBS
import qualified Data.Digest.CRC32        as DDC

import qualified PurpleMuon.Network.Types as PNT

-- | Prepend a message with an crc32 checksum.
prepend :: PNT.ProtocolUUID -> PNT.NakedMessage -> PNT.RawMessage
prepend uuid m = PNT.RawMessage prepMsg
  where
    crc32 = toS $ DBI.encode $ DDC.crc32 (uuid <> (PNT.unNakedMessage m))
    prepMsg = crc32 <> (PNT.unNakedMessage m)

-- | strip an crc32 off of a message.
-- Returns Just (stripped message) if the message contains the correct uuid
-- and Nothing otherwise
strip :: PNT.ProtocolUUID -> PNT.RawMessage -> Maybe PNT.NakedMessage
strip uuid m = if (crc32 == h) then (Just $ PNT.NakedMessage t) else Nothing
  where
    l = DBS.length uuid
    (h, t) = DBS.splitAt l (PNT.unRawMessage m)
    crc32 = toS $ DBI.encode $ DDC.crc32 (uuid <> t)

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
