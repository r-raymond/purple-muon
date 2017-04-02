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
    ( AppState(..), running, game, comState, frameState
    , Game
    , FrameState(..), fpsCounter, frameBegin, dt
    , Resources(..), window, renderer
    , FpsCounter(..)
    ) where

import           Protolude

import qualified Control.Lens             as CLE
import qualified Data.Thyme.Clock         as DTC
import qualified SDL.Video                as SVI

import qualified Client.States.Types      as CST
import qualified PurpleMuon.Physics.Types as PPT
import qualified PurpleMuon.Types         as PPY

-- | The `AppState` holds all information of the entire application.
data AppState
    = AppState
    { _running    :: Bool
    , _game       :: CST.State
    , _comState   :: CST.CommonState
    , _frameState :: FrameState
    }

-- | The frame state contains information on the app's fps.
data FrameState
    = FrameState
    { _fpsCounter :: FpsCounter
    , _frameBegin :: DTC.UTCTime
    , _dt         :: PPT.DeltaTime
    }

type Game a = ReaderT Resources (StateT AppState IO) a

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
CLE.makeLenses ''Resources
CLE.makeLenses ''FrameState
