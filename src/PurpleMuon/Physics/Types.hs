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
Module      : PurpleMuon.Physics.Types
Description : A collection of all types used in PupleMuon's physics code
Copyright   : (c) Robin Raymond, 2016
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}

{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module PurpleMuon.Physics.Types
    ( -- * Basic types
      PhyObjUUID(..)
    , FlType
    , Position(..)
    , Mass(..)
    , Velocity(..)
    , Acceleration(..)
    , DeltaTime(..)
    , Force(..)
    , GravitationalConstant(..)
    -- * Combined types
    , PhysicalObject(..), uuid, mass, pos, vel, static, gravitating
    , PhysicalSize(..)
    , Derivative(..)
    -- * IntMap types
    , Derivatives
    , PhysicalObjects
    , Forces
    -- * Boolean types
    , ObjGrav(..)
    , ObjType(..)
    ) where

import           Protolude

import qualified Control.Lens       as CLE
import qualified Data.AdditiveGroup as DAD
import qualified Data.Binary        as DBI
import qualified Data.IntMap.Strict as DIS
import qualified Data.VectorSpace   as DVE
import qualified Linear.V2          as LV2
import qualified Linear.Vector      as LVE


-- | A physical object identifier
newtype PhyObjUUID = PhyObjUUID { unPhyObjUUID :: Word16 }
    deriving (Eq, Ord)

-- |The floating point type used throughout the physics module
type FlType = Float

-- | The position of an object
newtype Position = Position { unPosition :: LV2.V2 FlType }
  deriving (Eq, Show, Generic)

-- | The mass of an object
newtype Mass = Mass { unMass :: FlType }
  deriving (Eq, Show, Generic)

-- | The velocity of an object
newtype Velocity = Velocity { unVelocity :: LV2.V2 FlType }
  deriving (Eq, Show, Generic)

-- | The acceleration of an object
newtype Acceleration = Acceleration { unAcceleration :: LV2.V2 FlType }
  deriving (Eq, Show, Generic)

-- | A time delta
-- Meassured in seconds
newtype DeltaTime = DeltaTime { unDeltaTime :: FlType }
  deriving (Eq, Show, Ord, Generic)

-- | A Force
newtype Force = Force { unForce :: LV2.V2 FlType }
  deriving (Eq, Show, Generic)

-- | The gravitaional constant
newtype GravitationalConstant = GravitationalConstant { unGravitationalConstant :: FlType }
  deriving (Eq, Show, Generic)

-- | The type of a physical object
data ObjType = Static       -- ^ the objects position is fixed
             | NonStatic    -- ^ the objects position is not fixed
    deriving (Eq, Show, Generic)

-- | The type of a gravitating body
data ObjGrav = Gravitating      -- ^ the object exerts gravitational forces
             | NonGravitating   -- ^ the object only recerives grav. forces
    deriving (Eq, Show, Generic)

-- | A physical object
data PhysicalObject
    = PhysicalObject
    { _uuid        :: Int      -- ^ A unique id of this object
    , _mass        :: Mass     -- ^ The mass of the object
    , _pos         :: Position -- ^ The position of the object
    , _vel         :: Velocity -- ^ The velocity of the object
    , _static      :: ObjType  -- ^ Is this object moving?
    , _gravitating :: ObjGrav  -- ^ Is this object gravitating?
    } deriving (Show, Generic)

instance (Eq PhysicalObject) where
    (==) a b = (_uuid a) == (_uuid b)

instance DBI.Binary Mass
instance DBI.Binary Position
instance DBI.Binary Velocity
instance DBI.Binary ObjType
instance DBI.Binary ObjGrav
instance DBI.Binary PhysicalObject


CLE.makeLenses ''PhysicalObject

-- |The physical size of the playing board
newtype PhysicalSize = PhysicalSize { unPhysicalSize :: LV2.V2 FlType }
  deriving (Eq, Show, Generic)

-- |The derivaties of a physical object numerical integrater
newtype Derivative = Derivative { unDerviative :: (Velocity, Force) }
    deriving(Eq, Show, Generic)

-- |A collection of derivaties, indexed by the `uuid` of the objects
type Derivatives = IntMap Derivative

-- |A collection of objects, indexed by the `uuid` of the objects
type PhysicalObjects = DIS.IntMap PhysicalObject

-- |A collection of forces, indexed by the `uuid` of the objects
type Forces = DIS.IntMap Force

-- Use CPP to define cumbersome instances
#define VECTORSPACE(TYPE) \
    instance (DAD.AdditiveGroup TYPE) where \
      { zeroV = TYPE LVE.zero \
      ; (^+^) (TYPE a) (TYPE b) = TYPE (a LVE.^+^ b) \
      ; negateV (TYPE a) = TYPE $ LVE.negated a  }; \
    instance (DVE.VectorSpace TYPE) where \
      { type (Scalar TYPE) = FlType \
      ; (*^) l (TYPE a) = TYPE (fmap (*l) a) };

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

instance (DAD.AdditiveGroup DeltaTime) where
    zeroV = DeltaTime 0
    (^+^) (DeltaTime a) (DeltaTime b) = DeltaTime (a + b)
    negateV (DeltaTime a) = DeltaTime ((-1) * a)
