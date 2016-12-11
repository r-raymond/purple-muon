{-|
Module      : PurpleMuon.Physics.Types
Description : A collection of all types used in PupleMuon's physics code
Copyright   : (c) Robin Raymond, 2016
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}

module PurpleMuon.Physics.Types
    ( Position
    , Mass
    , Velocity
    , Acceleration
    , DeltaTime
    ) where

import Protolude

import qualified Linear.V2 as LV2

-- | The position of an object
type Position = LV2.V2 Float

-- | The mass of an object
type Mass = Float

-- | The velocity of an object
type Velocity = LV2.V2 Float

-- | The acceleration of an object
type Acceleration = LV2.V2 Float

-- | A time delta
type DeltaTime = Float
