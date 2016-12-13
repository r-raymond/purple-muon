{-|
Module      : PurpleMuon.Physics.Types
Description : A collection of all types used in PupleMuon's physics code
Copyright   : (c) Robin Raymond, 2016
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}

module PurpleMuon.Physics.Types
    ( Position(..)
    , Mass(..)
    , Velocity(..)
    , Acceleration(..)
    , DeltaTime(..)
    , Force(..)
    , StaticObject(..)
    , GravitationalConstant(..)
    , DynamicObject(..)
    ) where

import Protolude

import qualified Linear.V2 as LV2

-- | The position of an object
newtype Position = Position { unPosition :: LV2.V2 Float }

-- | The mass of an object
newtype Mass = Mass { unMass :: Float }

-- | The velocity of an object
newtype Velocity = Velocity { unVelocity :: LV2.V2 Float }

-- | The acceleration of an object
newtype Acceleration = Acceleration { unAcceleration :: LV2.V2 Float }

-- | A time delta
newtype DeltaTime = DeltaTime { unDeltaTime :: Float }

-- | A Force
newtype Force = Force { unForce :: LV2.V2 Float }

-- | A static object
newtype StaticObject = StaticObject { unStaticObject :: (Position, Mass) }

-- | A dynamic object
newtype DynamicObject = DynamicObject { unDynamicObject :: (StaticObject, Velocity) }

-- | The gravitaional constant
newtype GravitationalConstant = GravitationalConstant { unGravitationalConstant :: Float }
