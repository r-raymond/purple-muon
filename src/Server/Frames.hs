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

-- TODO: Restrict state via zoom
frameBegin :: STY.Server ()
frameBegin = do
    t <- PUF.getTime
    modify $ CLE.set STY.frameBegin t

-- TODO: Restrict state via zoom
manageFps :: STY.Server ()
manageFps = do
    start <- fmap (CLE.view STY.frameBegin) get
    void $ PUF.manageFps minFrameTime start

minFrameTime :: DTC.NominalDiffTime
minFrameTime = DTC.fromSeconds (1 / 30 :: PPY.FlType)
