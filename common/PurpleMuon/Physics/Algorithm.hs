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
    ) where

import Protolude

import qualified Linear.Metric as LME

import qualified PurpleMuon.Physics.Types as PPT

integrateAccel :: PPT.DeltaTime -> PPT.Acceleration -> PPT.Velocity -> PPT.Velocity
integrateAccel (PPT.DeltaTime dt) (PPT.Acceleration a) (PPT.Velocity v) = PPT.Velocity (fmap (* dt) a + v)

integrateVel :: PPT.DeltaTime -> PPT.Velocity -> PPT.Position -> PPT.Position
integrateVel (PPT.DeltaTime dt) (PPT.Velocity v) (PPT.Position p) = PPT.Position (fmap (* dt) v + p)

newton2nd :: PPT.Force -> PPT.Mass -> PPT.Acceleration
newton2nd (PPT.Force f) (PPT.Mass m) = PPT.Acceleration (fmap (/ m) f)

integrateObject :: PPT.DeltaTime -> PPT.DynamicObject -> PPT.Force -> PPT.DynamicObject
integrateObject dt (PPT.DynamicObject (PPT.StaticObject (p, m), v)) f =
    PPT.DynamicObject ((PPT.StaticObject (pnew, m)), vnew)
      where
        a = newton2nd f m
        vnew = integrateAccel dt a v
        pnew = integrateVel dt vnew p

-- |The gravitational force between to objects
-- This only calculates the magnitude of the force
gravitationalForce :: PPT.GravitationalConstant -> PPT.StaticObject -> PPT.StaticObject -> Float
gravitationalForce (PPT.GravitationalConstant g)
                   (PPT.StaticObject (PPT.Position p1, PPT.Mass m1))
                   (PPT.StaticObject (PPT.Position p2, PPT.Mass m2)) =
    g * m1 * m2 / (LME.qd p1 p2)

-- |The graviational forces between to objects
-- Returns the gravitaitonal force on the first object. The force on the
-- second one is of course just the negative of the returned force.
gravitationalForceVec :: PPT.GravitationalConstant -> PPT.StaticObject -> PPT.StaticObject -> PPT.Force
gravitationalForceVec g
                      o1@(PPT.StaticObject (PPT.Position p1, _))
                      o2@(PPT.StaticObject (PPT.Position p2, _)) = PPT.Force $
    fmap (* (gravitationalForce g o1 o2)) (LME.signorm (p2 - p1))

-- | 
