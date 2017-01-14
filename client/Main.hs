module Main where

import           Protolude

import qualified Control.Concurrent.STM    as CCS
import qualified Data.IntMap.Strict        as DIS
import           Network.Socket.ByteString
import qualified SDL.Video                 as SVI

import qualified PurpleMuon.Network.Types  as PNT
import qualified PurpleMuon.Network.Util   as PNU
import qualified PurpleMuon.Physics.Types  as PPT

import qualified Client.Init               as CIN
import qualified Client.MainLoop           as CMA
import qualified Client.Types              as CTY

uuid :: PNT.UUID
uuid = PNT.UUID "Test"

initialeState :: CTY.AppState
initialeState =
    CTY.AppState True
        (CTY.GameState DIS.empty (PPT.DeltaTime 0) (PPT.DeltaTime 0))
        (CTY.FpsCounter 60 [])
        (toEnum 0)

game :: CCS.TBQueue PNT.NakedMessage -> SVI.Window -> SVI.Renderer -> IO ()
game tb w r = evalStateT (runReaderT CMA.initLoop (CTY.Resources w r tb)) initialeState

main :: IO ()
main = do
    (Right cs) <- PNU.clientSocket "127.0.0.1" "7123"
    _ <- send cs "Hello World"
    tb <- CCS.atomically $ CCS.newTBQueue 128
    _ <- forkIO $ PNU.endlessRecv uuid 1400 cs tb
    r <- CIN.withGraphics (game tb)
    case r of
        Left ex  -> putStrLn ("Error: " <> (show ex) :: Text)
        Right () -> return ()
    return ()
