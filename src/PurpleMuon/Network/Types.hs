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
Copyright   : (c) Robin Raymond, 2016-2017
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}
module PurpleMuon.Network.Types
    ( RawMessage(..)
    , NakedMessage(..)
    , ProtocolUUID
    , Payload(..)
    , MessageCount(..)
    , AckField(..)
    , Offset(..)
    , NetworkConfig(..)
    , NetworkState(..)
    , ConnectionState(..)
    , ServerToClientMsg(..)
    , ClientToServerMsg(..)
    , PlayerName(..)
    , UpdateMsg(..)
    ) where

import           Protolude

import qualified Data.Binary              as DBI
import qualified Network.Socket           as NSO

import qualified PurpleMuon.Game.Types    as PGT
import qualified PurpleMuon.Physics.Types as PPT

-- | A binary message that is send over the network
newtype RawMessage = RawMessage { unRawMessage :: ByteString }

-- | A binary message without a uuid header
newtype NakedMessage = NakedMessage { unNakedMessage :: ByteString }

-- | The payload of a Message
newtype Payload = Payload { unPayload :: ByteString }

-- | A uuid is a header that the server and client prepends to
-- every message. A message that does not have such a header is
-- discarded immediately
type ProtocolUUID = ByteString

-- | A counter to count both the remote and local messages
newtype MessageCount = MessageCount { unMessageCounter :: Word32 }
    deriving (Generic)

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

-- | Messages the Server sends to the Clients
data ServerToClientMsg
    = Ping              -- ^ A simple ping package to determine network latency
    | CreateGameObject
        ( PGT.GameObjKey
        , PGT.GameObject
        , Maybe PPT.PhysicalObject) -- ^ Create a new game object
    | Update PPT.PhysicalObjects    -- ^ The physical objects in the system
    -- TODO: Pack this better. Also send the generation of the data
    deriving (Generic)

-- | A player name
newtype PlayerName = PlayerName { unPlayerName :: Text }
    deriving (Generic)

-- | Messages the Clients send to the Server
data ClientToServerMsg
    = RequestConnection PlayerName
                        -- ^ Request a connection to the server. The sever will
                        -- answer with ping commands to establish the network
                        -- latency if the request is granted
    | Pong MessageCount -- ^ Answer to a ping command. The `MessageCount` indicates which
    | ClientUpdate UpdateMsg  -- ^ Regular updates the client sends to the server
        deriving (Generic) -- `Ping` is answered

data UpdateMsg
    = UpdateMsg
    { lastAck :: Word32
    , ackField :: Word32
    , controls :: Word8
    } deriving (Generic)


instance DBI.Binary MessageCount
instance DBI.Binary PlayerName
instance DBI.Binary ServerToClientMsg
instance DBI.Binary ClientToServerMsg
instance DBI.Binary UpdateMsg
