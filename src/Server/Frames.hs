module Server.Frames
    ( frameBegin
    , manageFps
    ) where


import           Protolude

import qualified Control.Lens             as CLE
import qualified Data.Thyme.Clock         as DTC

import qualified PurpleMuon.Types         as PPY
import qualified PurpleMuon.Util.Frames   as PUF

import qualified Server.Types             as STY

frameBegin :: STY.Server ()
frameBegin = PUF.frameBegin storeFb
  where
    storeFb t = modify $ CLE.set STY.frameBegin t

manageFps :: STY.Server ()
manageFps = PUF.manageFps minFrameTime getFb storeDt
  where
    getFb = fmap (CLE.view STY.frameBegin) get
    storeDt _ = return ()

minFrameTime :: DTC.NominalDiffTime
minFrameTime = DTC.fromSeconds (1 / 30 :: PPY.FlType)
