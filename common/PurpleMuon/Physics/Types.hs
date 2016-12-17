{-|
Module      : PurpleMuon.Physics.Types
Description : A collection of all types used in PupleMuon's physics code
Copyright   : (c) Robin Raymond, 2016
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}

{-# LANGUAGE TemplateHaskell #-}

module PurpleMuon.Physics.Types
    ( Position(..)
    , Mass(..)
    , Velocity(..)
    , Acceleration(..)
    , DeltaTime(..)
    , Force(..)
    , GravitationalConstant(..)
    , PhysicalObject(..), uuid, mass, pos, vel, PurpleMuon.Physics.Types.force, static, gravitating
    , PhysicalSize(..)
    ) where

import           Protolude

import qualified Control.Lens as CLE
import qualified Linear.V2    as LV2

-- | The position of an object
newtype Position = Position { unPosition :: LV2.V2 Float }
  deriving (Eq, Show)

-- | The mass of an object
newtype Mass = Mass { unMass :: Float }
  deriving (Eq, Show)

-- | The velocity of an object
newtype Velocity = Velocity { unVelocity :: LV2.V2 Float }
  deriving (Eq, Show)

-- | The acceleration of an object
newtype Acceleration = Acceleration { unAcceleration :: LV2.V2 Float }
  deriving (Eq, Show)

-- | A time delta
newtype DeltaTime = DeltaTime { unDeltaTime :: Float }
  deriving (Eq, Show)

-- | A Force
newtype Force = Force { unForce :: LV2.V2 Float }
  deriving (Eq, Show)

-- | The gravitaional constant
newtype GravitationalConstant = GravitationalConstant { unGravitationalConstant :: Float }
  deriving (Eq, Show)

-- | A physical object
data PhysicalObject
    = PhysicalObject
    { _uuid        :: Int      -- ^ A unique id of this object
    , _mass        :: Mass     -- ^ The mass of the object
    , _pos         :: Position -- ^ The position of the object
    , _vel         :: Velocity -- ^ The velocity of the object
    , _force       :: Force    -- ^ The current forces applied to this object
    , _static      :: Bool     -- ^ Is this object moving?
    , _gravitating :: Bool     -- ^ Is this object gravitating?
    } deriving (Show)

instance (Eq PhysicalObject) where
    (==) a b = (_uuid a) == (_uuid b)

CLE.makeLenses ''PhysicalObject

-- The physical size of the playing board
newtype PhysicalSize = PhysicalSize { unPhysicalSize :: LV2.V2 Float }
  deriving (Eq, Show)
