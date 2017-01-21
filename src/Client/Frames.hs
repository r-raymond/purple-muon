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
import qualified PurpleMuon.Util.Frames   as PUF

import qualified Client.Types             as CTY

frameBegin :: CTY.Game ()
frameBegin = PUF.frameBegin storeFb
  where
    storeFb x = modify (CLE.set CTY.frameBegin x)

manageFps :: CTY.Game ()
manageFps = PUF.manageFps minFrameTime getFb storeDt
  where
    getFb = fmap (CLE.view CTY.frameBegin) get
    storeDt dt = do
        modify (CLE.set (CTY.game . CTY.dt) (PPT.DeltaTime dt))
        modify (CLE.over CTY.fps (registerFps dt))
        modify (CLE.over (CTY.game . CTY.accumTime)
                         (\(PPT.DeltaTime a) -> PPT.DeltaTime (a + dt)))

registerFps :: PPT.FlType -> CTY.FpsCounter -> CTY.FpsCounter
registerFps fps fpsC = fpsC { CTY.fpsL = take m newFpsL }
  where
    m = CTY.maxFrames fpsC
    f = CTY.fpsL fpsC
    newFpsL = fps : f

getAvgFrametime :: CTY.FpsCounter -> PPT.FlType
getAvgFrametime  (CTY.FpsCounter _ f) = sum f / fromIntegral (max (length f) 1)

minFrameTime :: DTC.NominalDiffTime
minFrameTime = DTC.fromSeconds (1 / 120 :: PPT.FlType)

fpsFormat :: FOR.Format r (PPT.FlType -> r)
fpsFormat = "Fps : " FOR.% FOR.fixed 1

formatFps :: CTY.Game Text
formatFps = do
    st <- get
    let a = getAvgFrametime (CLE.view CTY.fps st)
    return (FOR.sformat fpsFormat (1/a))
