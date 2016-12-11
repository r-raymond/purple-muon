module PurpleMuon.Connection
    ( NetworkConfig(..)
    , NetworkState(..)
    , ConnectionState(..)
    , openConnection
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

-- | Open a UDP Socket and optionally specify a port
openConnection :: Maybe Text -> IO (Either SomeException NSO.Socket)
openConnection port = try $ do
    a:_ <- NSO.getAddrInfo Nothing (Just "localhost") (fmap toS port)
    NSO.socket (NSO.addrFamily a) NSO.Datagram NSO.defaultProtocol
