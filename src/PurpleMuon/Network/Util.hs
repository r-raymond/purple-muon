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
Module      : PurpleMuon.Network.Types
Description : A collection utilty functions of PurpleMuon's Network Code
Copyright   : (c) Robin Raymond, 2016
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}
module PurpleMuon.Network.Util
    ( serverSocket
    , clientSocket
    , endlessRecv
    ) where

import           Protolude

import qualified Control.Concurrent.STM     as CCS
import qualified Network.Socket             as NSO
import qualified Network.Socket.ByteString  as NSB

import qualified PurpleMuon.Network.Message as PNM
import qualified PurpleMuon.Network.Types   as PNT

-- | Open a UDP Socket on a specific port
serverSocket :: MonadIO m => Text -> m (Either SomeException NSO.Socket)
serverSocket port = liftIO $ try $ do
    -- Note that getAddrInfo either gives a nonempty list or raises an exception
    a:_ <- NSO.getAddrInfo
                (Just $ NSO.defaultHints { NSO.addrFlags = [NSO.AI_PASSIVE] })
                (Just "127.0.0.1")
                (Just $ toS port)
    sock <- NSO.socket (NSO.addrFamily a) NSO.Datagram NSO.defaultProtocol
    NSO.bind sock (NSO.addrAddress a)
    return sock


-- | Connect an UDP socket to a specific port
clientSocket :: MonadIO m
             => Text -- ^ The host to connect to
             -> Text -- ^ The port to connect to
             -> m (Either SomeException NSO.Socket)
clientSocket host port = liftIO $ try $ do
    a:_ <- NSO.getAddrInfo Nothing (Just $ toS host) (Just $ toS port)
    sock <- NSO.socket (NSO.addrFamily a) NSO.Datagram NSO.defaultProtocol
    NSO.connect sock (NSO.addrAddress a)
    return sock

-- | Endless receiving queue
-- forkIO this and give it a STM object to write stuff to. It will block on UDP
-- recv and as soon as something arrives write it to the output
endlessRecv :: MonadIO m
            => PNT.UUID     -- ^ the uuid used for filtering
            -> Int          -- ^ max number of bytes
            -> NSO.Socket   -- ^ socket to listen to
            -> CCS.TBQueue PNT.NakedMessage -- ^ stm tbqueue
            -> m ()
endlessRecv uuid m sock tb = do
    bin <- liftIO $ NSB.recv sock m
    liftIO $ CCS.atomically $
        case PNM.strip uuid (PNT.RawMessage bin) of
            Just n -> CCS.writeTBQueue tb n
            _      -> return ()
    endlessRecv uuid m sock tb
