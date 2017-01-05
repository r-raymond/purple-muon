module Client.Frames
    ( frameBegin
    , manageFps
    , formatFps
    ) where

import           Protolude

import qualified Formatting as FOR
import qualified Control.Concurrent       as CCO
import qualified Control.Lens             as CLE
import qualified Data.AdditiveGroup       as DAD
import qualified Data.AffineSpace         as DAF
import qualified Data.Thyme.Clock         as DTC

import qualified PurpleMuon.Physics.Types as PPT

import qualified Client.Types             as CTY

-- | Call at the beginning of a frame.
-- Updates the frameBegin timer.
frameBegin :: CTY.Game ()
frameBegin = do
    b <- liftIO $ DTC.getCurrentTime
    modify (CLE.set CTY.frameBegin b)

-- | Handle frame time.
-- The function will then calculate the remaining time, sleep the thread,
-- update the frame counter and the dt variable of the game state
manageFps :: CTY.Game ()
manageFps = do
    st <- get
    end <- liftIO $ DTC.getCurrentTime
    let start = CLE.view CTY.frameBegin st
        used = end DAF..-. start

    when (used < minFrameTime) (waitFor (minFrameTime DAD.^-^ used))
    final <- liftIO $ DTC.getCurrentTime
    let elapsed = final DAF..-. start
        frameTime = DTC.toSeconds elapsed :: PPT.FlType

    -- set the game dt according to the passed time
    modify (CLE.set (CTY.game . CTY.dt) (PPT.DeltaTime frameTime))
    modify (CLE.over CTY.fps (registerFps frameTime))



registerFps :: PPT.FlType -> CTY.FpsCounter -> CTY.FpsCounter
registerFps fps fpsC = fpsC { CTY.fpsL = (take m newFpsL) }
  where
    m = CTY.maxFrames fpsC
    f = CTY.fpsL fpsC
    newFpsL = fps : f

getAvgFrametime :: CTY.FpsCounter -> PPT.FlType
getAvgFrametime  (CTY.FpsCounter _ f) = (sum f) / (fromIntegral (max (length f) 1))

minFrameTime :: DTC.NominalDiffTime
minFrameTime = DTC.fromSeconds (1 / 60 :: PPT.FlType)

fpsFormat :: FOR.Format r (PPT.FlType -> r)
fpsFormat = "Fps : " FOR.% (FOR.fixed 1)

formatFps :: CTY.Game Text
formatFps = do
    st <- get
    let a = getAvgFrametime (CLE.view CTY.fps st)
    return (FOR.sformat fpsFormat (1/a))

waitFor :: MonadIO m => DTC.NominalDiffTime -> m ()
waitFor dt = liftIO $ CCO.threadDelay dt_ms
  where
    dt_s = DTC.toSeconds dt :: PPT.FlType
    dt_ms_float = dt_s * 1000000
    dt_ms = truncate dt_ms_float

