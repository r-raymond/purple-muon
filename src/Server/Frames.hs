module Server.Frames
    ( frameBegin
    , manageFps
    ) where


import           Protolude

import qualified Control.Lens             as CLE
import qualified Data.Thyme.Clock         as DTC

import qualified PurpleMuon.Physics.Types as PPT
import qualified PurpleMuon.Util.Frames   as PUF

import qualified Server.Types             as STY

frameBegin :: STY.Server ()
frameBegin = PUF.frameBegin storeFb
  where
    storeHelp :: DTC.UTCTime -> STY.ServerState -> STY.ServerState
    storeHelp t (STY.WaitingForConnections _) = STY.WaitingForConnections t
    storeHelp t (STY.InGame gs)               = STY.InGame $ CLE.set STY.frameBegin t gs
    storeFb x = modify (storeHelp x)

manageFps :: STY.Server ()
manageFps = PUF.manageFps minFrameTime getFb storeDt
  where
    getFbHelper :: STY.ServerState -> DTC.UTCTime
    getFbHelper (STY.WaitingForConnections t) = t
    getFbHelper (STY.InGame gs)               = CLE.view STY.frameBegin gs
    getFb = fmap getFbHelper get
    storeDt _ = return ()

minFrameTime :: DTC.NominalDiffTime
minFrameTime = DTC.fromSeconds (1 / 30 :: PPT.FlType)
