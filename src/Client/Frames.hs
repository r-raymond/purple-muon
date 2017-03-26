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
Module      : Client.Frames
Description : Manage frames client specifically
Copyright   : (c) Robin Raymond, 2016-2017
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}

module Client.Frames
    ( frameBegin
    , manageFps
    , formatFps
    ) where

import           Protolude

import qualified Control.Lens             as CLE
import qualified Data.Thyme.Clock         as DTC
import qualified Formatting               as FOR

import qualified PurpleMuon.Physics.Types as PPT
import qualified PurpleMuon.Types         as PTY
import qualified PurpleMuon.Util.Frames   as PUF

import qualified Client.Types             as CTY

-- | Manage the beginning of a frame, i.e. store the current time
frameBegin :: (MonadIO m, MonadState DTC.UTCTime m) => m ()
frameBegin = PUF.getTime >>= put

-- | Manage the end of a frame.
-- Sleeps, times and updates the frame counter
manageFps :: (MonadIO m, MonadState CTY.FrameState m) => m ()
manageFps = do
    start <- fmap (CLE.view CTY.frameBegin) get
    dt <- PUF.manageFps minFrameTime start

    modify (CLE.set CTY.dt (PPT.DeltaTime dt))
    modify (CLE.over CTY.fpsCounter (registerFps dt))

registerFps :: PTY.FlType -> CTY.FpsCounter -> CTY.FpsCounter
registerFps fps fpsC = fpsC { CTY.fpsL = take m newFpsL }
  where
    m = CTY.maxFrames fpsC
    f = CTY.fpsL fpsC
    newFpsL = fps : f

getAvgFrametime :: CTY.FpsCounter -> PTY.FlType
getAvgFrametime  (CTY.FpsCounter _ f) = sum f / fromIntegral (max (length f) 1)

minFrameTime :: DTC.NominalDiffTime
minFrameTime = DTC.fromSeconds (1 / 120 :: PTY.FlType)

fpsFormat :: FOR.Format r (PTY.FlType -> r)
fpsFormat = "Fps : " FOR.% FOR.fixed 1

formatFps :: CTY.FpsCounter -> Text
formatFps cou = FOR.sformat fpsFormat (1/a)
  where
    a = getAvgFrametime cou
