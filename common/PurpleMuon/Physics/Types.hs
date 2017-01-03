{-|
Module      : PurpleMuon.Physics.Types
Description : A collection of all types used in PupleMuon's physics code
Copyright   : (c) Robin Raymond, 2016
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}

{-# LANGUAGE TemplateHaskell, CPP, TypeFamilies #-}

module PurpleMuon.Physics.Types
    ( Position(..)
    , Mass(..)
    , Velocity(..)
    , Acceleration(..)
    , DeltaTime(..)
    , Force(..)
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
import qualified Data.VectorSpace   as DVE

-- |The floating point type used throughout the physics module
type FlType = Double

-- | The position of an object
newtype Position = Position { unPosition :: LV2.V2 FlType }
  deriving (Eq, Show)

-- | The mass of an object
newtype Mass = Mass { unMass :: FlType }
  deriving (Eq, Show)

-- | The velocity of an object
newtype Velocity = Velocity { unVelocity :: LV2.V2 FlType }
  deriving (Eq, Show)

-- | The acceleration of an object
newtype Acceleration = Acceleration { unAcceleration :: LV2.V2 FlType }
  deriving (Eq, Show)

-- | A time delta
newtype DeltaTime = DeltaTime { unDeltaTime :: FlType }
  deriving (Eq, Show)

-- | A Force
newtype Force = Force { unForce :: LV2.V2 FlType }
  deriving (Eq, Show)

-- | The gravitaional constant
newtype GravitationalConstant = GravitationalConstant { unGravitationalConstant :: FlType }
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
newtype PhysicalSize = PhysicalSize { unPhysicalSize :: LV2.V2 FlType }
  deriving (Eq, Show)

-- |The derivaties of a physical object numerical integrater
newtype Derivative = Derivative { unDerviative :: (Velocity, Force) }

-- |A collection of derivaties, indexed by the `uuid` of the objects
type Derivatives = IntMap Derivative

-- |A collection of objects, indexed by the `uuid` of the objects
type PhysicalObjects = DIS.IntMap PhysicalObject

-- |A collection of forces, indexed by the `uuid` of the objects
type Forces = DIS.IntMap Force

-- Use CPP to define cumbersome instances
#define VECTORSPACE(x) \
    instance (DAD.AdditiveGroup x) where \
      { zeroV = x LVE.zero \
      ; (^+^) (x a) (x b) = x (a LVE.^+^ b) \
      ; negateV (x a) = x $ LVE.negated a  }; \
    instance (DVE.VectorSpace x) where \
      { type (Scalar x) = FlType \
      ; (*^) l (x a) = x (fmap (*l) a) };

-- All vector quantities instance vectorspace (note, `Position` does NOT)
VECTORSPACE(Force)
VECTORSPACE(Velocity)
VECTORSPACE(Acceleration)

instance (DAD.AdditiveGroup Derivative) where
    zeroV = Derivative DAD.zeroV
    (^+^) (Derivative a) (Derivative b) = Derivative (a DAD.^+^ b)
    negateV (Derivative a) = Derivative $ DAD.negateV a

instance (DVE.VectorSpace Derivative) where
    type (Scalar Derivative) = FlType
    (*^) l (Derivative a) = Derivative (l DVE.*^ a)
