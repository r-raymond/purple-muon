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
    ) where

import Protolude

import qualified PurpleMuon.Physics.Types as PPT

integrateAccel :: PPT.DeltaTime -> PPT.Velocity -> PPT.Position -> PPT.Position
integrateAccel dt v p = fmap (* dt) v + p
