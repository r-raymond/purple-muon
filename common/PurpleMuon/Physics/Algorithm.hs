{-|
Module      : PurpleMuon.Physics.Algorithm
Description : Algorithms used by the Physics module of PurpleMuon
Copyright   : (c) Robin Raymond, 2016
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}

module PurpleMuon.Physics.Algorithm
    ( integrateTimeStep
    ) where

import           Protolude

import qualified Control.Lens                 as CLE
import qualified Data.AdditiveGroup           as DAD
import qualified Data.IntMap.Strict           as DIS
import qualified Data.VectorSpace             as DVE
import qualified Linear.Metric                as LME
import qualified Linear.V2                    as LV2
import qualified Linear.V4                    as LV4
import qualified Linear.Vector                as LVE

import qualified PurpleMuon.Physics.Constants as PPC
import qualified PurpleMuon.Physics.Types     as PPT

integrateAccel :: PPT.DeltaTime -> PPT.Acceleration -> PPT.Velocity -> PPT.Velocity
integrateAccel (PPT.DeltaTime dt) (PPT.Acceleration a) (PPT.Velocity v) = PPT.Velocity (fmap (* dt) a + v)

integrateVel :: PPT.DeltaTime -> PPT.Velocity -> PPT.Position -> PPT.Position
integrateVel (PPT.DeltaTime dt) (PPT.Velocity v) (PPT.Position p) = PPT.Position (fmap (* dt) v + p)

newton2nd :: PPT.Force -> PPT.Mass -> PPT.Acceleration
newton2nd (PPT.Force f) (PPT.Mass m) = PPT.Acceleration (fmap (/ m) f)


-- | Integrate a physical object system.
-- Uses RK4, and the notation from the wikipedia article
-- https://en.wikipedia.org/wiki/Runge%E2%80%93Kutta_methods
integrateTimeStep :: PPT.GravitationalConstant
                  -> PPT.DeltaTime          -- ^ the timestep
                  -> PPT.PhysicalObjects    -- ^ physical objects of the system
                  -> PPT.Forces             -- ^ external forces
                  -> PPT.PhysicalObjects
integrateTimeStep g dt yn exFor = ynp1
  where
    dt_half = PPT.DeltaTime $ (/2) $ PPT.unDeltaTime dt
    exDer   = fmap (\x -> PPT.Derivative (DAD.zeroV, x)) exFor
    k1'     = evaluatePhysics g (PPT.DeltaTime 0) yn DIS.empty
    k1      = DIS.unionWith (DAD.^+^) k1' exDer
    k2'     = evaluatePhysics g dt_half yn k1
    k2      = DIS.unionWith (DAD.^+^) k2' exDer
    k3'     = evaluatePhysics g dt_half yn k2
    k3      = DIS.unionWith (DAD.^+^) k3' exDer
    k4'     = evaluatePhysics g dt yn k3
    k4      = DIS.unionWith (DAD.^+^) k4' exDer
    finalD  = DIS.unionWith
                (DAD.^+^)
                (fmap ((DVE.*^) (1/6)) k1)
                (DIS.unionWith
                    (DAD.^+^)
                    (fmap ((DVE.*^) (1/3)) k2)
                    (DIS.unionWith
                        (DAD.^+^)
                        (fmap ((DVE.*^) (1/3)) k3)
                        (fmap ((DVE.*^) (1/6)) k4)))
    ynp1    = integratePhysics dt yn finalD

-- | Integrate a physical object.
-- Uses the force saved in the object and applies is to the physical object via
-- explicit Euler.
integrateObject :: PPT.DeltaTime -> PPT.PhysicalObject -> PPT.Derivative -> PPT.PhysicalObject
integrateObject dt po (PPT.Derivative (v, f)) =
    CLE.set PPT.pos pnew $
    CLE.set PPT.vel vnew po
      where
        m = CLE.view PPT.mass po
        p = CLE.view PPT.pos po
        a = newton2nd f m
        vnew = integrateAccel dt a v
        pnew = wrapTorus $ integrateVel dt v p

-- | Integrabte a physical system and derive new derivatives
-- First du a explicit euler integration of the physical objects with respect
-- to the given derivatives. Then calculate the new derivatives arrising from
-- the integrated state.
evaluatePhysics :: PPT.GravitationalConstant
                -> PPT.DeltaTime
                -> PPT.PhysicalObjects
                -> PPT.Derivatives
                -> PPT.Derivatives
evaluatePhysics g dt pos ders = newDer
  where
    newPos = integratePhysics dt pos ders
    newVel = fmap (CLE.view PPT.vel) newPos
    newFor = calculateGravitationalForces g newPos
    newVe' = fmap (\x -> PPT.Derivative (x, DAD.zeroV)) newVel
    newFo' = fmap (\x -> PPT.Derivative (DAD.zeroV, x)) newFor
    newDer = DIS.unionWith (DAD.^+^) newVe' newFo'


-- | Integrate a complete physical system
-- Basically just (fmap integrateObject)
integratePhysics :: PPT.DeltaTime -> PPT.PhysicalObjects -> PPT.Derivatives -> PPT.PhysicalObjects
integratePhysics dt pos ders = DIS.mapWithKey helper pos
  where
    helper :: Int -> PPT.PhysicalObject -> PPT.PhysicalObject
    helper key obj = integrateObject dt obj (defaultLookup key obj ders)

-- | Lookup the derivative of an object given a key
defaultLookup :: Int -> PPT.PhysicalObject -> PPT.Derivatives -> PPT.Derivative
defaultLookup key obj ders = DIS.findWithDefault (zeroD obj) key ders
  where
    zeroD :: PPT.PhysicalObject -> PPT.Derivative
    zeroD o = PPT.Derivative $ (CLE.view PPT.vel o, PPT.Force LVE.zero)


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

-- | Calculate all forces for a system.
calculateGravitationalForces :: PPT.GravitationalConstant -- ^ Gravitational Constant
                             -> PPT.PhysicalObjects
                             -> PPT.Forces
calculateGravitationalForces g objs = forceMap
      where
        gravitatingObjs = (DIS.filter (CLE.view PPT.gravitating) objs)
        nonStaticObjs   = (DIS.filter (not . (CLE.view PPT.static)) objs)
        -- Apply all forces
        foldForces :: PPT.PhysicalObject -> PPT.Force
        foldForces x = DIS.foldr' (\o facc -> facc DAD.^+^ calculateForce g o x) (PPT.Force $ LVE.zero) gravitatingObjs
        forceMap = DIS.map foldForces nonStaticObjs

-- | Calculate the force of the first object on the second
-- Ignores all static / gravitating modifier but checks that the objects
-- are not the same
calculateForce :: PPT.GravitationalConstant
               -> PPT.PhysicalObject -- ^ The object that gravitates
               -> PPT.PhysicalObject -- ^ The object that is pulled on
               -> PPT.Force
calculateForce g o1 o2 =
    if o1 /= o2
        then f
        else PPT.Force $ LVE.zero
      where
        f = gravitationalForce PPC.physicalSize g o2 o1

wrap :: PPT.FlType -> PPT.FlType -> PPT.FlType
wrap bound x
    | x < 0     = wrap bound (x + bound)
    | x > bound = wrap bound (x - bound)
    | otherwise = x

wrapTorus :: PPT.Position -> PPT.Position
wrapTorus = PPT.Position . cutOff . PPT.unPosition
  where
    PPT.PhysicalSize (LV2.V2 xMax yMax) = PPC.physicalSize
    cutOff :: LV2.V2 PPT.FlType -> LV2.V2 PPT.FlType
    cutOff (LV2.V2 x y) = LV2.V2 (wrap xMax x) (wrap yMax y)
