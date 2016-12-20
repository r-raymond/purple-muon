module Main where

import           Protolude

import           Network.Socket.ByteString

import           PurpleMuon.Network.Util
import qualified PurpleMuon.Physics.Types  as PPT

import qualified Client.Init               as CIN
import qualified Client.MainLoop           as CMA
import qualified Client.Types              as CTY

objs :: [PPT.PhysicalObject]
objs = []

initialeState :: CTY.AppState
initialeState = CTY.AppState True (CTY.GameState objs)

main :: IO ()
main = do
    (Right cs) <- clientSocket "127.0.0.1" "7123"
    _ <- send cs "Hello World"
    r <- CIN.withGraphics (\x y -> (evalStateT (CMA.loop x y) initialeState))
    case r of
        Left ex  -> putStrLn ("Error: " <> (show ex) :: Text)
        Right () -> return ()
    return ()
