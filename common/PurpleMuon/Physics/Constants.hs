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
    , minimumDistance
    ) where

import Protolude

import qualified Linear.V2                as LV2

import qualified PurpleMuon.Physics.Types as PPT

g :: PPT.GravitationalConstant
g = PPT.GravitationalConstant 1

physicalSize :: PPT.PhysicalSize
physicalSize = PPT.PhysicalSize (LV2.V2 1 1)

-- | The minimum distance two objects are allowed to have
-- This makes the integration numerically much more stable
minimumDistance :: Float
minimumDistance = 0.05
