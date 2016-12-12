{-|
Module      : PurpleMuon.Physics.Algorithm
Description : Algorithms used by the Physics module of PurpleMuon
Copyright   : (c) Robin Raymond, 2016
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}

module PurpleMuon.Physics.Algorithm
    ( integrateAccel
    , integrateVel
    , gravitationalForce
    ) where

import Protolude

import qualified Linear.Metric as LME

import qualified PurpleMuon.Physics.Types as PPT

integrateAccel :: PPT.DeltaTime -> PPT.Acceleration -> PPT.Velocity -> PPT.Velocity
integrateAccel dt a v = fmap (* dt) a + v

integrateVel :: PPT.DeltaTime -> PPT.Velocity -> PPT.Position -> PPT.Position
integrateVel dt v p = fmap (* dt) v + p

-- |The gravitational force between to objects
-- This only calculates the magnitude of the force
gravitationalForce :: Float -> PPT.StaticObject -> PPT.StaticObject -> Float
gravitationalForce g (p1, m1) (p2, m2) = g * m1 * m2 / (LME.qd p1 p2)
