module PurpleMuon.Connection
    (
    ) where

import Protolude

import qualified Network.Socket as NSO

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
    { addr :: NSO.SockAddr
    , latestCounter :: PNE.MessageCount
    }
