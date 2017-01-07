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
serverSocket :: Text -> IO (Either SomeException NSO.Socket)
serverSocket port = try $ do
    -- Note that getAddrInfo either gives a nonempty list or raises an exception
    a:_ <- NSO.getAddrInfo
                (Just $ NSO.defaultHints { NSO.addrFlags = [NSO.AI_PASSIVE] })
                (Just "127.0.0.1")
                (Just $ toS port)
    sock <- NSO.socket (NSO.addrFamily a) NSO.Datagram NSO.defaultProtocol
    NSO.bind sock (NSO.addrAddress a)
    return sock


-- | Connect an UDP socket to a specific port
clientSocket :: Text -- ^ The host to connect to
             -> Text -- ^ The port to connect to
             -> IO (Either SomeException NSO.Socket)
clientSocket host port = try $ do
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
