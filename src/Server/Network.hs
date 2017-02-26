
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

module Server.Network
    (
    ) where

import           Protolude

import qualified Control.Lens              as CLE
import qualified Data.Binary               as DBI
import qualified Data.ByteString           as DBY
import qualified Data.Digest.CRC32         as DDC
import qualified Network.Socket.ByteString as NSB
import qualified System.Log.FastLogger     as SLF

import qualified PurpleMuon.Network.Types  as PNT
import qualified Server.Types              as STY

-- | Send a package to all clients. Don't request an acknowledgment of arrival.
sendPackage :: (MonadState STY.GameState m, MonadReader STY.Resources m, MonadIO m)
            => PNT.ServerToClientMsg
            -> m ()
sendPackage pkg = do
    st <- get
    res <- ask
    let bin = toS $ DBI.encode pkg
        message = (toS $ DBI.encode $ DDC.crc32 (protocolUUID <> bin)) <> bin
        socket = CLE.view STY.socket res
        logger = CLE.view STY.logger res
        send = liftIO $ NSB.sendTo socket message
        clients = CLE.view STY.clients st


    liftIO $ SLF.pushLogStrLn logger (SLF.toLogStr ("Sending package. Size: "
                                    <> (show $ DBY.length message)
                                    <> "; Clients: "
                                    ++  (show $ length clients)))
    sequence_ (fmap send (fmap (CLE.view STY.addr) clients))
