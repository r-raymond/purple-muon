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
    , calculateGravitationalForces
    , resetForces
    , gravitationalForce
    ) where

import           Protolude

import qualified Control.Lens             as CLE
import qualified Linear.Metric            as LME
import qualified Linear.V2                as LV2
import qualified Linear.V4                as LV4

import qualified PurpleMuon.Physics.Constants as PPC
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
        dtHalf = PPT.DeltaTime $ (/ 2) $ PPT.unDeltaTime dt
        a = newton2nd (CLE.view PPT.force po) (CLE.view PPT.mass po)
        vnew = integrateAccel dt a (CLE.view PPT.vel po)
        p_1p5 = integrateVel dtHalf (CLE.view PPT.vel po) (CLE.view PPT.pos po)
        pnew = integrateVel dtHalf vnew p_1p5

-- | Calculate the gravitational forces between two objects
-- The logic is as follow. First find the coordinates of the two objects
-- on the two-torus. Then embedds that torus (flat) into $\mathbb{R}^4$ and
-- calculates the forces in there. The third step is to project the forces onto
-- the torus and finally push it back to $\R^2$.
--
-- The returned force is the force to apply to the first of the two objects.
-- Obviously the force on the other one is the negative of that.
--
-- This function uses the minimum distance defined in `Constants`
gravitationalForce :: PPT.PhysicalSize
                   -> PPT.GravitationalConstant
                   -> PPT.PhysicalObject
                   -> PPT.PhysicalObject
                   -> PPT.Force
gravitationalForce ps g o1 o2 = PPT.Force $ LV2.V2 projectedForce1 projectedForce2
  where
    p1 = PPT.unPosition $ (CLE.view PPT.pos) o1
    p2 = PPT.unPosition $ (CLE.view PPT.pos) o2
    m1 = PPT.unMass $ (CLE.view PPT.mass) o1
    m2 = PPT.unMass $ (CLE.view PPT.mass) o2
    gVal = PPT.unGravitationalConstant g
    LV2.V2 xMax yMax = PPT.unPhysicalSize ps
    -- Calculate positions in the 2-torus parametrized by two angles from 0 to
    -- 2*pi
    phi1 = ((CLE.view LV2._x) p1) * 2 * pi / xMax
    phi2 = ((CLE.view LV2._y) p1) * 2 * pi / yMax

    psi1 = ((CLE.view LV2._x) p2) * 2 * pi / xMax
    psi2 = ((CLE.view LV2._y) p2) * 2 * pi / yMax
    -- Calculate Position in $\R^4$. Notice that we use the normed two-torus,
    -- because the final push forward cancels the xMax, yMax coefficients
    q1 = LV4.V4 (cos phi1) (sin phi1) (cos phi2) (sin phi2)
    q2 = LV4.V4 (cos psi1) (sin psi1) (cos psi2) (sin psi2)
    -- Calculate force in $\R^4$
    dist = max (LME.distance q1 q2) PPC.minimumDistance
    direction  = q2 - q1

    coeff= gVal * m1 * m2 / (dist * dist * dist)
    LV4.V4 fw fx fy fz = fmap (* coeff) direction
    force1 = LV2.V2 fw fx
    force2 = LV2.V2 fy fz
    -- Project the force onto the torus
    unitVec1 = LV2.V2 ((-1)*(sin phi1)) (cos phi1)
    unitVec2 = LV2.V2 ((-1)*(sin phi2)) (cos phi2)
    projectedForce1 = (force1 `LME.dot` unitVec1)
    projectedForce2 = (force2 `LME.dot` unitVec2)
    -- Technically I need to still push back here, i.e. multiply both vectors
    -- with 1/(2*pi). However, let's just compensate this with g.


addForce :: PPT.Force -> PPT.Force -> PPT.Force
addForce (PPT.Force f1) (PPT.Force f2) = PPT.Force (f1 + f2)

-- | Reset all forces to 0
-- , gravitationalForce
resetForces :: [PPT.PhysicalObject] -> [PPT.PhysicalObject]
resetForces = fmap (CLE.set PPT.force (PPT.Force $ LV2.V2 0 0))

-- | Calculate all forces for a system.
-- Does not reset the forces, needs to be done manually before
calculateGravitationalForces :: PPT.GravitationalConstant -- ^ Gravitational Constant
                             -> [PPT.PhysicalObject]      -- ^ The physical objects to integrate
                             -> [PPT.PhysicalObject]
calculateGravitationalForces g pos = staticObjs ++ newNonStaticObjs
      where
        staticObjs      = filter (CLE.view PPT.static) pos
        gravitatingObjs = filter (CLE.view PPT.gravitating) pos
        nonStaticObjs   = filter (not . (CLE.view PPT.static)) pos
        -- Apply all forces
        newNonStaticObjs = fmap (\x -> foldr' (applyForce g) x gravitatingObjs) nonStaticObjs


-- |Apply the force of the first object to the second
-- Ignores all static / gravitating modifier but checks that the objects
-- are not the same
applyForce :: PPT.GravitationalConstant -> PPT.PhysicalObject -> PPT.PhysicalObject -> PPT.PhysicalObject
applyForce g o1 o2 =
    if o1 /= o2
        then CLE.over PPT.force (addForce f) o2
        else o2
      where
        f = gravitationalForce PPC.physicalSize g o2 o1
