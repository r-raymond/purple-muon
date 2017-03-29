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
Module      : Client.States.InGameState.Types
Description : The types used for the state in game.
Copyright   : (c) Robin Raymond, 2016-2017
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}

{-# LANGUAGE TemplateHaskell #-}

module Client.States.InGameState.Types
    ( State(..), physicalObjects, accumTime, gameObjects, controls, netState,
                 keymap, gameFonts, gameSprites
    , NetworkState(..), lastPacket, lastID, ackField, socket, tbqueue
    ) where

import qualified Control.Concurrent.STM   as CCS
import qualified Control.Lens             as CLE
import qualified Data.IntMap.Strict       as DIS
import qualified Data.Thyme.Clock         as DTC
import qualified Network.Socket           as NSO

import qualified Client.Assets.Font       as CAF
import qualified Client.Assets.Sprite     as CAS
import qualified PurpleMuon.Game.Types    as PGT
import qualified PurpleMuon.Input.Types   as PIT
import qualified PurpleMuon.Network.Types as PNT
import qualified PurpleMuon.Physics.Types as PPT

data State
    = State
    { _physicalObjects :: PPT.PhysicalObjects
    , _accumTime       :: PPT.DeltaTime         -- ^ Accumulated time for fixed physics step
    , _gameObjects     :: DIS.IntMap PGT.GameObject
    , _controls        :: PIT.Controls
    , _netState        :: NetworkState
    , _keymap          :: PIT.KeyMap
    , _gameSprites     :: CAS.SpriteLoaderType
    , _gameFonts       :: CAF.FontLoaderType
    }

-- | The network state of a client.
-- This data type contains every information that is available for a connection
-- to a game server.
data NetworkState
    = NetworkState
    { _lastPacket :: DTC.UTCTime
    , _lastID     :: PNT.MessageCount
    , _ackField   :: PNT.AckField
    , _socket     :: NSO.Socket
    , _tbqueue    :: CCS.TBQueue PNT.ServerToClientMsg
    }

CLE.makeLenses ''State
CLE.makeLenses ''NetworkState
