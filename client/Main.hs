module Main where

import           Protolude

import qualified Data.IntMap.Strict        as DIS
import qualified Linear.V2                 as LV2
import qualified Network.Socket            as NSO
import           Network.Socket.ByteString
import qualified SDL.Video                 as SVI

import           PurpleMuon.Network.Util
import qualified PurpleMuon.Physics.Types  as PPT

import qualified Client.Init               as CIN
import qualified Client.MainLoop           as CMA
import qualified Client.Types              as CTY

objs :: PPT.PhysicalObjects
objs = DIS.fromList
        [ (1, PPT.PhysicalObject 1 (PPT.Mass 1) (PPT.Position (LV2.V2 0.5 0.5)) (PPT.Velocity (LV2.V2 0 (-0.1)))  PPT.NonStatic PPT.Gravitating)
        , (2, PPT.PhysicalObject 2 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.5)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (3, PPT.PhysicalObject 3 (PPT.Mass 1) (PPT.Position (LV2.V2 0.25 0.5)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (4, PPT.PhysicalObject 4 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.25)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (5, PPT.PhysicalObject 5 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.35)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (6, PPT.PhysicalObject 6 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.45)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (7, PPT.PhysicalObject 7 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.55)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (8, PPT.PhysicalObject 8 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.65)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (9, PPT.PhysicalObject 9 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.75)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (10, PPT.PhysicalObject 10 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.85)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (11, PPT.PhysicalObject 11 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.95)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (12, PPT.PhysicalObject 12 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.05)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (13, PPT.PhysicalObject 13 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.15)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (14, PPT.PhysicalObject 14 (PPT.Mass 1) (PPT.Position (LV2.V2 0.15 0.5)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (15, PPT.PhysicalObject 15 (PPT.Mass 1) (PPT.Position (LV2.V2 0.35 0.5)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (16, PPT.PhysicalObject 16 (PPT.Mass 1) (PPT.Position (LV2.V2 0.45 0.5)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (17, PPT.PhysicalObject 17 (PPT.Mass 1) (PPT.Position (LV2.V2 0.55 0.5)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)]

initialeState :: CTY.AppState
initialeState = CTY.AppState True
                             (CTY.GameState objs (PPT.DeltaTime 0) (PPT.DeltaTime 0))
                             (CTY.FpsCounter 60 [])
                             (toEnum 0)

game :: NSO.Socket -> SVI.Window -> SVI.Renderer -> IO ()
game s w r = evalStateT (runReaderT CMA.loop (CTY.Resources w r s)) initialeState

main :: IO ()
main = do
    (Right cs) <- clientSocket "127.0.0.1" "7123"
    _ <- send cs "Hello World"
    r <- CIN.withGraphics (game cs)
    case r of
        Left ex  -> putStrLn ("Error: " <> (show ex) :: Text)
        Right () -> return ()
    return ()
