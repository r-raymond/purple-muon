module Server.Config
    ( initialObjs
    , initialPhyObjs
    ) where

import           Protolude

import qualified Data.IntMap.Strict       as DIS
import qualified Linear.V2                as LV2

import qualified PurpleMuon.Game.Types    as PGT
import qualified PurpleMuon.Physics.Types as PPT

initialObjs :: DIS.IntMap PGT.GameObject
initialObjs = DIS.fromList
        [ (1, PGT.GameObject PGT.Comet Nothing (Just (PPT.PhyObjUUID 1)))
        , (2, PGT.GameObject PGT.Comet Nothing (Just (PPT.PhyObjUUID 2)))
        , (3, PGT.GameObject PGT.Comet Nothing (Just (PPT.PhyObjUUID 3)))
        , (4, PGT.GameObject PGT.Comet Nothing (Just (PPT.PhyObjUUID 4)))
        , (5, PGT.GameObject PGT.Comet Nothing (Just (PPT.PhyObjUUID 5)))
        , (6, PGT.GameObject PGT.Comet Nothing (Just (PPT.PhyObjUUID 6)))
        , (7, PGT.GameObject PGT.Comet Nothing (Just (PPT.PhyObjUUID 7))) ]



initialPhyObjs :: PPT.PhysicalObjects
initialPhyObjs = DIS.fromList
        [ (1, PPT.PhysicalObject
                (PPT.Mass 1)
                (PPT.Position (LV2.V2 0.5 0.5))
                (PPT.Velocity (LV2.V2 0 (-0.1)))
                PPT.NonStatic
                PPT.Gravitating)
        , (2, PPT.PhysicalObject
                (PPT.Mass 1)
                (PPT.Position (LV2.V2 0.75 0.5))
                (PPT.Velocity (LV2.V2 0 0.1))
                PPT.NonStatic
                PPT.Gravitating)
        , (3, PPT.PhysicalObject
                (PPT.Mass 1)
                (PPT.Position (LV2.V2 0.75 0.15))
                (PPT.Velocity (LV2.V2 0 0.1))
                PPT.NonStatic
                PPT.Gravitating)
        , (4, PPT.PhysicalObject
                (PPT.Mass 1)
                (PPT.Position (LV2.V2 0.75 0.25))
                (PPT.Velocity (LV2.V2 0 0.1))
                PPT.NonStatic
                PPT.Gravitating)
        , (5, PPT.PhysicalObject
                (PPT.Mass 1)
                (PPT.Position (LV2.V2 0.75 0.35))
                (PPT.Velocity (LV2.V2 0 0.1))
                PPT.NonStatic
                PPT.Gravitating)
        , (6, PPT.PhysicalObject
                (PPT.Mass 1)
                (PPT.Position (LV2.V2 0.75 0.45))
                (PPT.Velocity (LV2.V2 0 0.1))
                PPT.NonStatic
                PPT.Gravitating)
        , (7, PPT.PhysicalObject
                (PPT.Mass 1)
                (PPT.Position (LV2.V2 0.75 0.55))
                (PPT.Velocity (LV2.V2 0 0.1))
                PPT.NonStatic
                PPT.Gravitating)]

