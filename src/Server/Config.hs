module Server.Config
    ( initialObjs
    ) where

import           Protolude

import qualified Data.Vector.Generic      as DVG
import qualified Data.Vector.Mutable      as DVM
import qualified Linear.V2                as LV2

import qualified PurpleMuon.Game.Types    as PGT
import qualified PurpleMuon.Physics.Types as PPT

initialObjs :: MonadIO m => m (DVM.IOVector PGT.GameObject)
initialObjs = liftIO $ DVG.fromList []

--        [ PGT.GameObject Nothing
--                         (Just $ PPT.PhysicalObject 1
--                            (PPT.Mass 1)
--                            (PPT.Position (LV2.V2 0.5 0.5))
--                            (PPT.Velocity (LV2.V2 0 (-0.1)))
--                            PPT.NonStatic
--                            PPT.Gravitating)
--        , PGT.GameObject Nothing
--                         (Just $ PPT.PhysicalObject 2
--                            (PPT.Mass 1)
--                            (PPT.Position (LV2.V2 0.75 0.5))
--                            (PPT.Velocity (LV2.V2 0 0.1))
--                            PPT.NonStatic
--                            PPT.Gravitating)
--        , PGT.GameObject Nothing
--                         (Just $ PPT.PhysicalObject 3
--                            (PPT.Mass 1)
--                            (PPT.Position (LV2.V2 0.75 0.15))
--                            (PPT.Velocity (LV2.V2 0 0.1))
--                            PPT.NonStatic
--                            PPT.Gravitating)
--        , PGT.GameObject Nothing
--                         (Just $ PPT.PhysicalObject 4
--                            (PPT.Mass 1)
--                            (PPT.Position (LV2.V2 0.75 0.25))
--                            (PPT.Velocity (LV2.V2 0 0.1))
--                            PPT.NonStatic
--                            PPT.Gravitating)
--        , PGT.GameObject Nothing
--                         (Just $ PPT.PhysicalObject 5
--                            (PPT.Mass 1)
--                            (PPT.Position (LV2.V2 0.75 0.35))
--                            (PPT.Velocity (LV2.V2 0 0.1))
--                            PPT.NonStatic
--                            PPT.Gravitating)
--        , PGT.GameObject Nothing
--                         (Just $ PPT.PhysicalObject 6
--                            (PPT.Mass 1)
--                            (PPT.Position (LV2.V2 0.75 0.45))
--                            (PPT.Velocity (LV2.V2 0 0.1))
--                            PPT.NonStatic
--                            PPT.Gravitating)
--        , PGT.GameObject Nothing
--                         (Just $ PPT.PhysicalObject 7
--                            (PPT.Mass 1)
--                            (PPT.Position (LV2.V2 0.75 0.55))
--                            (PPT.Velocity (LV2.V2 0 0.1))
--                            PPT.NonStatic
--                            PPT.Gravitating)]
--
