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
    ) where

import Protolude


import qualified Network.Socket     as NSO

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
