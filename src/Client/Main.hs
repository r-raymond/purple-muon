--  Copyright 2016, 2017 Robin Raymond
--
--  This file is part of Purple Muon
--
--  Purple Muon is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  Purple Muon is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Purple Muon.  If not, see <http://www.gnu.org/licenses/>.

module Client.Main where

import           Protolude

import           Version

import qualified Data.Binary                  as DBI
import qualified SDL

import qualified PurpleMuon.Network.Types     as PNT
import qualified PurpleMuon.Physics.Types     as PPT

import qualified Client.Init                  as CIN
import qualified Client.Loop                  as CLO
import qualified Client.States.MenuState.Init as CSMI
import qualified Client.States.Types          as CST
import qualified Client.Types                 as CTY
import qualified Client.Video.Types           as CVT

uuid :: PNT.ProtocolUUID
uuid = toS $ DBI.encode (1337 :: Word32)

initialeState :: MonadIO m => SDL.Renderer -> m CTY.AppState
initialeState r = do
    menuState <- CSMI.initMenu r
    return $ CTY.AppState True menuState
        (CST.CommonState
            (CVT.Resolution $ SDL.V2 800 600)
            [])
        (CTY.FrameState
            (CTY.FpsCounter 60 [])
            (toEnum 0)
            (PPT.DeltaTime 0))

game :: SDL.Window -> SDL.Renderer -> IO ()
game w r = do
    initS <- initialeState r
    evalStateT (runReaderT CLO.loop (CTY.Resources w r)) initS

main :: IO ()
main = do
    putStrLn ("Purple Muon " <> gitTag <> "\nbuild for "
            <> platform <> " by " <> compiler)
    --(Right cs) <- PNU.clientSocket "127.0.0.1" "7123"
    --let cRe = PNT.RequestConnection (PNT.PlayerName "Chopstick WarrioR")
    --_ <- NSB.send cs (toS (DBI.encode cRe))
    --tb <- CCS.atomically $ CCS.newTBQueue 128
    --_ <- forkIO $ PNU.endlessRecv uuid 1400 cs tb
    r <- CIN.withGraphics game
    case r of
        Left ex  -> putStrLn ("Error: " <> (show ex) :: Text)
        Right () -> return ()
    return ()
