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

{-# LANGUAGE TemplateHaskell #-}

module Server.Types
    ( WaitingState(..)
    , GameState(..), gObjs, pObjs, frameBegin, clients, logger, intStep
    , WaitingServer, clientsConnected
    , Server
    , Resources(..), tbqueue, socket, uuid
    , ClientConnection(..), addr, name, gameObj
    ) where

import           Protolude

import qualified Control.Concurrent.STM   as CCS
import qualified Control.Lens             as CLE
import qualified Data.IntMap.Strict       as DIS
import qualified Data.Thyme.Clock         as DTC
import qualified Network.Socket           as NSO
import qualified System.Log.FastLogger    as SLF

import qualified PurpleMuon.Game.Types    as PGT
import qualified PurpleMuon.Network.Types as PNT
import qualified PurpleMuon.Physics.Types as PPT

-- | A client connection saves all the data the server knows about
-- a client
data ClientConnection
    = ClientConnection
    { _addr    :: NSO.SockAddr
    , _name    :: Text
    , _gameObj :: PGT.GameObjKey
    }

-- | The state of a server waiting for connections
data WaitingState
    = WaitingState
    { _clientsConnected :: [ClientConnection]
    }

-- | The state of a server in game
data GameState
    = GameState
    { _gObjs      :: DIS.IntMap PGT.GameObject
    , _pObjs      :: PPT.PhysicalObjects
    , _frameBegin :: DTC.UTCTime
    , _clients    :: [ClientConnection]
    , _intStep    :: Word32                     -- ^ Count of integration steps
    }

-- | Read only resources that the server has access to
data Resources
    = Resources
    { _tbqueue :: CCS.TBQueue PNT.NakedMessage
    , _socket  :: NSO.Socket
    , _logger  :: SLF.LoggerSet
    , _uuid    :: PNT.ProtocolUUID
    }

type WaitingServer a = ReaderT Resources (StateT WaitingState IO) a
type Server a = ReaderT Resources (StateT GameState IO) a

CLE.makeLenses ''ClientConnection
CLE.makeLenses ''WaitingState
CLE.makeLenses ''GameState
CLE.makeLenses ''Resources
