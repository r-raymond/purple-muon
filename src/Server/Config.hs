module Server.Config
    ( initialObjs
    ) where

import qualified Data.IntMap.Strict           as DIS
import qualified Linear.V2                    as LV2

import qualified PurpleMuon.Physics.Types as PPT

initialObjs :: PPT.PhysicalObjects
initialObjs = DIS.fromList
        [ (1, PPT.PhysicalObject 1 (PPT.Mass 1) (PPT.Position (LV2.V2 0.5 0.5)) (PPT.Velocity (LV2.V2 0 (-0.1)))  PPT.NonStatic PPT.Gravitating)
        , (2, PPT.PhysicalObject 2 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.5)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (3, PPT.PhysicalObject 3 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.15)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (4, PPT.PhysicalObject 4 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.25)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (5, PPT.PhysicalObject 5 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.35)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (6, PPT.PhysicalObject 6 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.45)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (7, PPT.PhysicalObject 7 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.55)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)]

