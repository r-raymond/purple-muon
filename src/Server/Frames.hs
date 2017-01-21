module Server.Frames
    ( frameBegin
    , manageFps
    ) where


import Protolude

import PurpleMuon.Util.Frames

frameBegin :: STY.Server
frameBegin = PUF.frameBegin storeFb
  where
    storeHelp :: DTC.UTCTime -> STY.ServerState -> STY.ServerState
    storeHelp t (STY.WaitingForConnections _) = STY.WaitingForConnections t
    storeHelp t (STY.InGame gs) = CLE.set 
    storeFb x = modify
