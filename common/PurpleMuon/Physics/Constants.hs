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
g = PPT.GravitationalConstant 0.1

physicalSize :: PPT.PhysicalSize
physicalSize = PPT.PhysicalSize (LV2.V2 1 1)

-- |Do physics at 30 Hz
physicsStep :: PPT.DeltaTime
physicsStep = PPT.DeltaTime (1 / 30)

-- | The minimum distance two objects are allowed to have
-- This makes the integration numerically much more stable
minimumDistance :: PPT.FlType
minimumDistance = 0.1
