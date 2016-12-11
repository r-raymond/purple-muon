module PurpleMuon.Network.Types
    ( RawMessage(..)
    , NakedMessage(..)
    , UUID(..)
    , MessageCount(..)
    , AckField(..)
    , Offset(..)
    , NetworkConfig(..)
    , NetworkState(..)
    , ConnectionState(..)
    ) where

import Protolude

import qualified Network.Socket     as NSO

-- | A binary message that is send over the network
newtype RawMessage = RawMessage { unRawMessage :: ByteString }

-- | A binary message without a uuid header
newtype NakedMessage = NakedMessage { unNakedMessage :: ByteString }

-- | A uuid is a header that the server and client prepends to
-- every message. A message that does not have such a header is
-- discarded immidieately
newtype UUID = UUID { unUUID :: ByteString }

-- | A counter to count both the remote and local messages
newtype MessageCount = MessageCount { unMessageCounter :: Word32 }

-- | A field indicating which of the last 32 messages have arrived.
newtype AckField = AckField { unAckField :: Word32 }

-- | A difference between to Message counts
newtype Offset = Offset { unOffset :: Int }

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
    , latestCounter :: MessageCount
    }

