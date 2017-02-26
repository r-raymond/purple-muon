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
    ( AppState(..), running, game, fps, frameBegin, textures
    , Game
    , GameState(..), physicalObjects, dt, accumTime
    , Resources(..), window, renderer, tbqueue
    , FpsCounter(..)
    , TextureUUIDs(..)
    ) where

import           Protolude

import qualified Control.Concurrent.STM   as CCS
import qualified Control.Lens             as CLE
import qualified Data.Thyme.Clock         as DTC
import qualified SDL.Video                as SVI

import qualified Client.Video.Types       as CVT
import qualified PurpleMuon.Network.Types as PNT
import qualified PurpleMuon.Physics.Types as PPT

data AppState
    = AppState
    { _running    :: Bool
    , _game       :: GameState
    , _fps        :: FpsCounter
    , _frameBegin :: DTC.UTCTime -- TODO: figure out how to get show back on Appstate
    , _textures   :: CVT.TextureLoader
    }

type Game a = ReaderT Resources (StateT AppState IO) a

data GameState
    = InGameState
    { _physicalObjects :: PPT.PhysicalObjects
    , _dt              :: PPT.DeltaTime
    , _accumTime       :: PPT.DeltaTime         -- ^ Accumulated time for fixed physics step
    }
    | MenuState

data Resources
    = Resources
    { _window   :: SVI.Window
    , _renderer :: SVI.Renderer
    , _tbqueue  :: CCS.TBQueue PNT.NakedMessage
    }

-- TODO: Make this more efficient. Maybe a mutable array?
data FpsCounter
    = FpsCounter
    { maxFrames :: Int
    , fpsL      :: [PPT.FlType]
    } deriving (Show)

data TextureUUIDs
    = TextureUUIDs
    { background :: CVT.TexUUID
    , stones     :: CVT.TexUUID
    }

CLE.makeLenses ''AppState
CLE.makeLenses ''GameState
CLE.makeLenses ''Resources
