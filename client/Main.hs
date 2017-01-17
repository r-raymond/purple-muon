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

module Main where

import           Protolude

import           Version

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
import qualified Client.Video.Texture      as CVT

uuid :: PNT.UUID
uuid = PNT.UUID "Test"

initialeState :: SVI.Renderer -> CTY.AppState
initialeState r =
    CTY.AppState True
        (CTY.GameState DIS.empty (PPT.DeltaTime 0) (PPT.DeltaTime 0))
        (CTY.FpsCounter 60 [])
        (toEnum 0)
        (CVT.newTextureLoader r)

game :: CCS.TBQueue PNT.NakedMessage -> SVI.Window -> SVI.Renderer -> IO ()
game tb w r = evalStateT (runReaderT CMA.initLoop (CTY.Resources w r tb)) (initialeState r)

main :: IO ()
main = do
    putStrLn ("Purple Muon " <> gitTag <> "\nbuild for "
            <> platform <> " by " <> compiler)
    (Right cs) <- PNU.clientSocket "127.0.0.1" "7123"
    _ <- send cs "Hello World"
    tb <- CCS.atomically $ CCS.newTBQueue 128
    _ <- forkIO $ PNU.endlessRecv uuid 1400 cs tb
    r <- CIN.withGraphics (game tb)
    case r of
        Left ex  -> putStrLn ("Error: " <> (show ex) :: Text)
        Right () -> return ()
    return ()
