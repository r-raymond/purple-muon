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
Module      : PurpleMuon.Physics.Constants
Description : Constants used by the Physics module of PurpleMuon
Copyright   : (c) Robin Raymond, 2016
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}

module PurpleMuon.Physics.Constants
    ( g
    , physicalSize
    , physicsStep
    , minimumDistance
    ) where

import Protolude

import qualified Linear.V2                as LV2

import qualified PurpleMuon.Physics.Types as PPT

g :: PPT.GravitationalConstant
g = PPT.GravitationalConstant 0.01

physicalSize :: PPT.PhysicalSize
physicalSize = PPT.PhysicalSize (LV2.V2 1 1)

-- |Do physics at 30 Hz
physicsStep :: PPT.DeltaTime
physicsStep = PPT.DeltaTime (1 / 30)

-- | The minimum distance two objects are allowed to have
-- This makes the integration numerically much more stable
minimumDistance :: PPT.FlType
minimumDistance = 0.1
