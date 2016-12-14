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
    ) where

import Protolude

import qualified Linear.Metric as LME
import qualified Linear.V2 as LV2

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

dynToSt :: PPT.DynamicObject -> PPT.StaticObject
dynToSt (PPT.DynamicObject (s, _)) = s

grToSt :: PPT.GravitatingObject -> PPT.StaticObject
grToSt (PPT.GravitatingObject x) = dynToSt x

addForce :: PPT.Force -> PPT.Force -> PPT.Force
addForce (PPT.Force f1) (PPT.Force f2) = PPT.Force (f1 + f2)

-- | Calculate all forces for a system.
-- For performance reasons we differentiate between objects that only receive
-- gravitating forces and objects that both receive and induce gravitating
-- forces
calculateGravitationalForces :: PPT.GravitationalConstant -- ^ Gravitational Constant
                             -> [PPT.StaticObject] -- ^ Objects that only gravitate
                             -> [PPT.GravitatingObject] -- ^ Objects that both gravitate and move
                             -> [PPT.DynamicObject] -- ^ Objects that only move
                             -> ( [(PPT.GravitatingObject, PPT.Force)]
                                , [(PPT.DynamicObject, PPT.Force)] )
calculateGravitationalForces g so go ro = dynamicForces g grWithF2 roWithF2
    where
      goWithF = fmap (\x -> (x, PPT.Force $ LV2.V2 0 0)) go
      roWithF = fmap (\x -> (x, PPT.Force $ LV2.V2 0 0)) ro
      (grWithF2, roWithF2) = staticForces g so goWithF roWithF


-- | Helper function for only the static forces
staticForces :: PPT.GravitationalConstant
            -> [PPT.StaticObject]
            -> [(PPT.GravitatingObject, PPT.Force)]
            -> [(PPT.DynamicObject, PPT.Force)]
            -> ( [(PPT.GravitatingObject, PPT.Force)]
               , [(PPT.DynamicObject, PPT.Force)] )
staticForces g so go ro = (graObj, dynObj)
  where
    helper :: PPT.StaticObject -> (PPT.DynamicObject, PPT.Force) -> (PPT.DynamicObject, PPT.Force)
    helper = foldingForceHelper g identity dynToSt
    helper2 :: PPT.StaticObject -> (PPT.GravitatingObject, PPT.Force) -> (PPT.GravitatingObject, PPT.Force)
    helper2 = foldingForceHelper g identity grToSt
    dynObj = fmap (\x -> foldr' helper x so) ro
    graObj = fmap (\x -> foldr' helper2 x so) go

-- | Helper function for only the dynamic forces
dynamicForces :: PPT.GravitationalConstant
              -> [(PPT.GravitatingObject, PPT.Force)]
              -> [(PPT.DynamicObject, PPT.Force)]
              -> ( [(PPT.GravitatingObject, PPT.Force)]
                 , [(PPT.DynamicObject, PPT.Force)] )
dynamicForces g go ro = (graObj, dynObj)
  where
    helper :: PPT.GravitatingObject -> (PPT.DynamicObject, PPT.Force) -> (PPT.DynamicObject, PPT.Force)
    helper = foldingForceHelper g grToSt dynToSt
    helper2 :: PPT.GravitatingObject -> (PPT.GravitatingObject, PPT.Force) -> (PPT.GravitatingObject, PPT.Force)
    helper2 = foldingForceHelper g grToSt grToSt
    dynObj = fmap (\x -> foldr' helper x (map fst go)) ro
    graObj = fmap (\x -> foldr' helper2 x (map fst go)) go -- TODO: Avoid calculating gravitational forces on oneself


foldingForceHelper :: PPT.GravitationalConstant -> (a -> PPT.StaticObject) -> (b -> PPT.StaticObject) -> a -> (b, PPT.Force) -> (b, PPT.Force)
foldingForceHelper g xts yts x (y, f) = (y, addForce f (gravitationalForceVec g (yts y) (xts x)))
