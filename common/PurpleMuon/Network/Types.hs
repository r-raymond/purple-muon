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
Module      : PurpleMuon.Network.Types
Description : A collection of all types used in PupleMuon's Network Code
Copyright   : (c) Robin Raymond, 2016
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}
module PurpleMuon.Network.Types
    ( RawMessage(..)
    , NakedMessage(..)
    , UUID(..)
    , Payload(..)
    , MessageCount(..)
    , AckField(..)
    , Offset(..)
    , NetworkConfig(..)
    , NetworkState(..)
    , ConnectionState(..)
    ) where

import Protolude

import qualified Network.Socket     as NSO

-- | A binary message that is send over the network
newtype RawMessage = RawMessage { unRawMessage :: ByteString }

-- | A binary message without a uuid header
newtype NakedMessage = NakedMessage { unNakedMessage :: ByteString }

-- | The payload of a Message
newtype Payload = Payload { unPayload :: ByteString }

-- | A uuid is a header that the server and client prepends to
-- every message. A message that does not have such a header is
-- discarded immidieately
newtype UUID = UUID { unUUID :: ByteString }

-- | A counter to count both the remote and local messages
newtype MessageCount = MessageCount { unMessageCounter :: Word32 }

-- | A field indicating which of the last 32 messages have arrived.
newtype AckField = AckField { unAckField :: Word32 }

-- | A difference between to Message counts
newtype Offset = Offset { unOffset :: Int }

-- | A Configuration of a connection
data NetworkConfig
    = ConnectionConfig
    { maxClients :: Int
    }

data NetworkState
    = NetworkState
    { socket :: NSO.Socket
    }

data ConnectionState
    = ConnectionState
    { addr          :: NSO.SockAddr
    , latestCounter :: MessageCount
    }

