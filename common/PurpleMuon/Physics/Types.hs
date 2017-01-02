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
    , ForceT(..), Force
    , GravitationalConstant(..)
    , PhysicalObject(..), uuid, mass, pos, vel, static, gravitating
    , PhysicalSize(..)
    , Derivatives
    , PhysicalObjects
    , Forces
    , Derivative(..)
    ) where

import           Protolude

import qualified Control.Lens       as CLE
import qualified Data.IntMap.Strict as DIS
import qualified Linear.V2          as LV2
import qualified Linear.Vector      as LVE
import qualified Data.AdditiveGroup as DAD

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
newtype ForceT a = Force { unForce :: LV2.V2 a }
  deriving (Eq, Show, Functor)

type Force = ForceT Float

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
    , _static      :: Bool     -- ^ Is this object moving?
    , _gravitating :: Bool     -- ^ Is this object gravitating?
    } deriving (Show)

instance (Eq PhysicalObject) where
    (==) a b = (_uuid a) == (_uuid b)

CLE.makeLenses ''PhysicalObject

-- |The physical size of the playing board
newtype PhysicalSize = PhysicalSize { unPhysicalSize :: LV2.V2 Float }
  deriving (Eq, Show)

-- |The derivaties of a physical object numerical integrater
newtype Derivative = Derivative { unDerviative :: (Velocity, Force) }

-- |A collection of derivaties, indexed by the `uuid` of the objects
type Derivatives = IntMap Derivative

-- |A collection of objects, indexed by the `uuid` of the objects
type PhysicalObjects = DIS.IntMap PhysicalObject

-- |A collection of forces, indexed by the `uuid` of the objects
type Forces = DIS.IntMap Force

instance (DAD.AdditiveGroup Derivative) where
    zeroV = Derivative LVE.zero
    (^+^) (Derivative a) (Derivative b) = Derivative (a LVE.^+^ b)
    negateV (Derivative a) = Derivative $ LVE.negated a

instance (DVE.VectorSpace Derivative) where
    (*^) a b = 
