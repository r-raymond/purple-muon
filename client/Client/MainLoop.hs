module Client.MainLoop
    ( loop
    ) where

import           Protolude

import qualified Control.Concurrent as CCO
import qualified Control.Lens       as CLE
import qualified Data.AdditiveGroup as DAD
import qualified Data.AffineSpace   as DAF
import qualified Data.Thyme.Clock   as DTC
import qualified Foreign.C.Types    as FCT
import qualified SDL                as SDL
import qualified SDL.Event          as SEV
import qualified SDL.Vect           as SVE
import qualified SDL.Video          as SVI

import qualified Client.Event       as CEV
import qualified Client.Types       as CTY

loop :: SVI.Window -> SVI.Renderer -> CTY.Game ()
loop window renderer = do
    start <- liftIO $ DTC.getCurrentTime

    SEV.mapEvents CEV.handleEvent
    SVI.rendererDrawColor renderer SDL.$= SVE.V4 0 0 0 0
    SVI.clear renderer

    SVI.rendererDrawColor renderer SDL.$= SVE.V4 255 0 0 0
    let help = fmap FCT.CInt (SVE.V2 10 10)
    SVI.fillRect renderer (Just (SVI.Rectangle (SVE.P help) help))

    SVI.present renderer
    end <- liftIO $ DTC.getCurrentTime
    let elapsed = end DAF..-. start
    when (elapsed < minLoopTime) (waitFor (minLoopTime DAD.^-^ elapsed))
    whenM (fmap (CLE.view CTY.running) get) (loop window renderer)

minLoopTime :: DTC.NominalDiffTime
minLoopTime = DTC.fromSeconds (1 / 60 :: Float)


waitFor :: MonadIO m => DTC.NominalDiffTime -> m ()
waitFor dt = liftIO $ CCO.threadDelay dt_ms
  where
    dt_s = DTC.toSeconds dt :: Float
    dt_ms_float = dt_s * 1000000
    dt_ms = truncate dt_ms_float
