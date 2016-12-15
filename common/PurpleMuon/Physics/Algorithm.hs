{-|
Module      : PurpleMuon.Physics.Algorithm
Description : Algorithms used by the Physics module of PurpleMuon
Copyright   : (c) Robin Raymond, 2016
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}

module PurpleMuon.Physics.Algorithm
    ( integrateObject
    , gravitationalForceVec
    , calculateGravitationalForces
    , resetForces
    ) where

import           Protolude

import qualified Control.Lens             as CLE
import qualified Linear.Metric            as LME
import qualified Linear.V2                as LV2

import qualified PurpleMuon.Physics.Types as PPT

integrateAccel :: PPT.DeltaTime -> PPT.Acceleration -> PPT.Velocity -> PPT.Velocity
integrateAccel (PPT.DeltaTime dt) (PPT.Acceleration a) (PPT.Velocity v) = PPT.Velocity (fmap (* dt) a + v)

integrateVel :: PPT.DeltaTime -> PPT.Velocity -> PPT.Position -> PPT.Position
integrateVel (PPT.DeltaTime dt) (PPT.Velocity v) (PPT.Position p) = PPT.Position (fmap (* dt) v + p)

newton2nd :: PPT.Force -> PPT.Mass -> PPT.Acceleration
newton2nd (PPT.Force f) (PPT.Mass m) = PPT.Acceleration (fmap (/ m) f)

-- | Integrate a physical object.
-- Uses the force saved in the object and applies is to the physical object
-- So far ignore the fact if the object is static. Should this be changed?
integrateObject :: PPT.DeltaTime -> PPT.PhysicalObject -> PPT.PhysicalObject
integrateObject dt po =
    CLE.set PPT.pos pnew $
    CLE.set PPT.vel vnew po
      where
        a = newton2nd (CLE.view PPT.force po) (CLE.view PPT.mass po)
        vnew = integrateAccel dt a (CLE.view PPT.vel po)
        pnew = integrateVel dt vnew (CLE.view PPT.pos po)

-- |The gravitational force between to objects
-- This only calculates the magnitude of the force
gravitationalForce :: PPT.GravitationalConstant -> PPT.PhysicalObject -> PPT.PhysicalObject -> Float
gravitationalForce (PPT.GravitationalConstant g) po1 po2 =
    g * m1 * m2 / (LME.qd p1 p2)
      where
        m1 = PPT.unMass $ CLE.view PPT.mass po1
        m2 = PPT.unMass $ CLE.view PPT.mass po2
        p1 = PPT.unPosition $ CLE.view PPT.pos po1
        p2 = PPT.unPosition $ CLE.view PPT.pos po2

-- |The graviational forces between to objects
-- Returns the gravitaitonal force on the first object. The force on the
-- second one is of course just the negative of the returned force.
gravitationalForceVec :: PPT.GravitationalConstant -> PPT.PhysicalObject -> PPT.PhysicalObject -> PPT.Force
gravitationalForceVec g po1 po2 = PPT.Force $
    fmap (* (gravitationalForce g po1 po2)) (LME.signorm (p2 - p1))
      where
        p1 = PPT.unPosition $ CLE.view PPT.pos po1
        p2 = PPT.unPosition $ CLE.view PPT.pos po2

addForce :: PPT.Force -> PPT.Force -> PPT.Force
addForce (PPT.Force f1) (PPT.Force f2) = PPT.Force (f1 + f2)

-- | Reset all forces to 0
resetForces :: [PPT.PhysicalObject] -> [PPT.PhysicalObject]
resetForces = fmap (CLE.set PPT.force (PPT.Force $ LV2.V2 0 0))

-- | Calculate all forces for a system.
-- Does not reset the forces, needs to be done manually before
calculateGravitationalForces :: PPT.GravitationalConstant -- ^ Gravitational Constant
                             -> [PPT.PhysicalObject]      -- ^ The physical objects to integrate
                             -> [PPT.PhysicalObject]
calculateGravitationalForces g pos = undefined
      where
        staticObjs      = filter (CLE.view PPT.static) pos
        nonStaticObjs   = filter (not . (CLE.view PPT.static)) pos
        gravitatingObjs = filter (CLE.view PPT.gravitating) nonStaticObjs
        dynamicObjs     = filter (not . (CLE.view PPT.gravitating)) nonStaticObjs


-- |Apply the force of the first object to the second
-- Ignores all static / gravitating modifier but checks that the objects
-- are not the same
applyForce :: PPT.GravitationalConstant -> PPT.PhysicalObject -> PPT.PhysicalObject -> PPT.PhysicalObject
applyForce g o1 o2 =
    if o1 /= o2
        then CLE.over PPT.force (addForce f) o2
        else o2
      where
        f = gravitationalForceVec g o1 o2
