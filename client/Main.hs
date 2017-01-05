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
        , (2, PPT.PhysicalObject 2 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.5)) (PPT.Velocity (LV2.V2 0 0.1)) False True)]

initialeState :: CTY.AppState
initialeState = CTY.AppState True
                             (CTY.GameState objs (PPT.DeltaTime 0))
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
