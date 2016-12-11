module PurpleMuon.Connection
    ( NetworkConfig(..)
    , NetworkState(..)
    , ConnectionState(..)
    , serverSocket
    , clientSocket
    ) where

import           Protolude

import qualified Network.Socket     as NSO

import qualified PurpleMuon.Network as PNE

-- | A Configuration of a connection
data NetworkConfig
    = ConnectionConfig
    { maxClients :: Int
    }

data NetworkState
    = NetworkState
    { socket :: NSO.Socket
    }

data ConnectionState
    = ConnectionState
    { addr          :: NSO.SockAddr
    , latestCounter :: PNE.MessageCount
    }

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
