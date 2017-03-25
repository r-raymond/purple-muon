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

{-|
Module      : Server.WaitingLoop
Description : The waiting loop of the server until all players connected
Copyright   : (c) Robin Raymond, 2016-2017
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}

module Server.WaitingLoop
    ( waitingLoop
    ) where

import           Protolude

import qualified Control.Lens              as CLE
import qualified Data.Binary               as DBI
import qualified Network.Socket.ByteString as NSB

import qualified PurpleMuon.Network.Types  as PNT
import qualified PurpleMuon.Types          as PTY
import qualified System.Log.FastLogger     as SLF

import qualified Server.Config             as SCO
import qualified Server.MainLoop           as SMA
import qualified Server.Types              as STY

waitingLoop :: STY.WaitingServer ()
waitingLoop = do
    res <- ask
    let s = CLE.view STY.socket res
        l = CLE.view STY.logger res
    (m, a) <- liftIO $ NSB.recvFrom s 1024
    let result = DBI.decodeOrFail $ toS $ m
    case result of
        Left _ -> do
            liftIO $ SLF.pushLogStrLn l "Discarding invalid package"
            waitingLoop
        Right (_, _, mes) ->
            case mes of
                (PNT.RequestConnection (PNT.PlayerName name)) -> liftIO $ do
                    SLF.pushLogStrLn l (SLF.toLogStr $ "Player " <> name <> " connected")
                    evalStateT (runReaderT SMA.loop res)
                               (STY.GameState SCO.initialObjs SCO.initialPhyObjs (toEnum 0) [STY.ClientConnection a name (PTY.Key 0)] 0)
                _ -> do
                    liftIO $ SLF.pushLogStrLn l "Discard valid but inappropriate package"
                    waitingLoop

