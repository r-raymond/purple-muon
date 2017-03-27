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

module Client.Types
    ( AppState(..), running, game, sprites, frameState, fonts
    , Game
    , FrameState(..), fpsCounter, frameBegin
    , NetworkState(..), lastPacket, lastID, ackField, socket, tbqueue
    , GameState(..), physicalObjects, dt, accumTime, gameObjects, controls, netState, keymap
    , Resources(..), window, renderer
    , FpsCounter(..)
    ) where

import           Protolude

import qualified Control.Concurrent.STM   as CCS
import qualified Control.Lens             as CLE
import qualified Data.IntMap.Strict       as DIS
import qualified Data.Thyme.Clock         as DTC
import qualified Network.Socket           as NSO
import qualified SDL.Video                as SVI

import qualified Client.Assets.Font       as CAF
import qualified Client.Assets.Sprite     as CAS
import qualified PurpleMuon.Game.Types    as PGT
import qualified PurpleMuon.Input.Types   as PIT
import qualified PurpleMuon.Network.Types as PNT
import qualified PurpleMuon.Physics.Types as PPT
import qualified PurpleMuon.Types         as PPY

-- | The `AppState` holds all information of the entire application.
data AppState
    = AppState
    { _running    :: Bool
    , _game       :: GameState
    , _sprites    :: CAS.SpriteLoaderType
    , _frameState :: FrameState
    , _fonts      :: CAF.FontLoaderType
    }

-- | The frame state contains information on the app's fps.
data FrameState
    = FrameState
    { _fpsCounter :: FpsCounter
    , _frameBegin :: DTC.UTCTime
    , _dt         :: PPT.DeltaTime
    }

type Game a = ReaderT Resources (StateT AppState IO) a

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

-- | `GameState` has the information of the current state of the game. In can be
-- either a ingame state, or a menu state.
data GameState
    = InGameState
    { _physicalObjects :: PPT.PhysicalObjects
    , _accumTime       :: PPT.DeltaTime         -- ^ Accumulated time for fixed physics step
    , _gameObjects     :: DIS.IntMap PGT.GameObject
    , _controls        :: PIT.Controls
    , _netState        :: NetworkState
    , _keymap          :: PIT.KeyMap
    }
    | MenuState

-- | Resources are things that do not change during a complete app cycle.
data Resources
    = Resources
    { _window   :: SVI.Window
    , _renderer :: SVI.Renderer
    }


-- TODO: Make this more efficient. Maybe a mutable array?
data FpsCounter
    = FpsCounter
    { maxFrames :: Int
    , fpsL      :: [PPY.FlType]
    } deriving (Show)

CLE.makeLenses ''AppState
CLE.makeLenses ''GameState
CLE.makeLenses ''Resources
CLE.makeLenses ''FrameState
CLE.makeLenses ''NetworkState
