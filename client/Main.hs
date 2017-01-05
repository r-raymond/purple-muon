module Main where

import           Protolude

import           Network.Socket.ByteString
import qualified Linear.V2    as LV2
import qualified SDL.Video                 as SVI
import qualified Data.IntMap.Strict           as DIS

import           PurpleMuon.Network.Util
import qualified PurpleMuon.Physics.Types  as PPT

import qualified Client.Init               as CIN
import qualified Client.MainLoop           as CMA
import qualified Client.Types              as CTY

objs :: PPT.PhysicalObjects
objs = DIS.fromList
        [ (1, PPT.PhysicalObject 1 (PPT.Mass 1) (PPT.Position (LV2.V2 0.5 0.5)) (PPT.Velocity (LV2.V2 0 (-0.1)))  False True)
        , (2, PPT.PhysicalObject 2 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.5)) (PPT.Velocity (LV2.V2 0 0.1)) False True)
        , (3, PPT.PhysicalObject 3 (PPT.Mass 1) (PPT.Position (LV2.V2 0.25 0.5)) (PPT.Velocity (LV2.V2 0 0.1)) False True)
        , (4, PPT.PhysicalObject 4 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.25)) (PPT.Velocity (LV2.V2 0 0.1)) False True)
        , (5, PPT.PhysicalObject 5 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.35)) (PPT.Velocity (LV2.V2 0 0.1)) False True)
        , (6, PPT.PhysicalObject 6 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.45)) (PPT.Velocity (LV2.V2 0 0.1)) False True)
        , (7, PPT.PhysicalObject 7 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.55)) (PPT.Velocity (LV2.V2 0 0.1)) False True)
        , (8, PPT.PhysicalObject 8 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.65)) (PPT.Velocity (LV2.V2 0 0.1)) False True)
        , (9, PPT.PhysicalObject 9 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.75)) (PPT.Velocity (LV2.V2 0 0.1)) False True)
        , (10, PPT.PhysicalObject 10 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.85)) (PPT.Velocity (LV2.V2 0 0.1)) False True)
        , (11, PPT.PhysicalObject 11 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.95)) (PPT.Velocity (LV2.V2 0 0.1)) False True)
        , (12, PPT.PhysicalObject 12 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.05)) (PPT.Velocity (LV2.V2 0 0.1)) False True)
        , (13, PPT.PhysicalObject 13 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.15)) (PPT.Velocity (LV2.V2 0 0.1)) False True)
        , (14, PPT.PhysicalObject 14 (PPT.Mass 1) (PPT.Position (LV2.V2 0.15 0.5)) (PPT.Velocity (LV2.V2 0 0.1)) False True)
        , (15, PPT.PhysicalObject 15 (PPT.Mass 1) (PPT.Position (LV2.V2 0.35 0.5)) (PPT.Velocity (LV2.V2 0 0.1)) False True)
        , (16, PPT.PhysicalObject 16 (PPT.Mass 1) (PPT.Position (LV2.V2 0.45 0.5)) (PPT.Velocity (LV2.V2 0 0.1)) False True)
        , (17, PPT.PhysicalObject 17 (PPT.Mass 1) (PPT.Position (LV2.V2 0.55 0.5)) (PPT.Velocity (LV2.V2 0 0.1)) False True)]

initialeState :: CTY.AppState
initialeState = CTY.AppState True
                             (CTY.GameState objs (PPT.DeltaTime 0) (PPT.DeltaTime 0))
                             (CTY.FpsCounter 60 [])
                             (toEnum 0)

game :: SVI.Window -> SVI.Renderer -> IO ()
game w r = evalStateT (runReaderT CMA.loop (CTY.Resources w r)) initialeState

main :: IO ()
main = do
    (Right cs) <- clientSocket "127.0.0.1" "7123"
    _ <- send cs "Hello World"
    r <- CIN.withGraphics game
    case r of
        Left ex  -> putStrLn ("Error: " <> (show ex) :: Text)
        Right () -> return ()
    return ()
