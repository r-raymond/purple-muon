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

import qualified PurpleMuon.Physics.Types as PPT
import qualified PurpleMuon.Physics.Algorithm as PPA
import qualified PurpleMuon.Physics.Constants as PPC

import qualified Client.Event       as CEV
import qualified Client.Types       as CTY

loop :: CTY.Game ()
loop = do
    res <- ask
    let window   = CLE.view CTY.window   res

    start <- liftIO $ DTC.getCurrentTime

    SEV.mapEvents CEV.handleEvent
    render

    advanceGameState

    end <- liftIO $ DTC.getCurrentTime
    let used = end DAF..-. start

    when (used < minLoopTime) (waitFor (minLoopTime DAD.^-^ used))
    final <- liftIO $ DTC.getCurrentTime
    let elapsed = final DAF..-. start

    SVI.windowTitle window SDL.$= (formatTitle elapsed)
    modify (CLE.set (CTY.game . CTY.dt) (PPT.DeltaTime $ DTC.toSeconds elapsed))
    whenM (fmap (CLE.view CTY.running) get) loop

minLoopTime :: DTC.NominalDiffTime
minLoopTime = DTC.fromSeconds (1 / 60 :: Float)

formatTitle :: DTC.NominalDiffTime -> Text
formatTitle dt = "FPS: " <> (show (1 / ((DTC.toSeconds dt) :: Float)))

waitFor :: MonadIO m => DTC.NominalDiffTime -> m ()
waitFor dt = liftIO $ CCO.threadDelay dt_ms
  where
    dt_s = DTC.toSeconds dt :: Float
    dt_ms_float = dt_s * 1000000
    dt_ms = truncate dt_ms_float

render :: CTY.Game ()
render = do
    res <- ask
    let renderer = CLE.view CTY.renderer res
    SVI.rendererDrawColor renderer SDL.$= SVE.V4 0 0 0 0
    SVI.clear renderer

    appState <- get
    let pos = CLE.view (CTY.game . CTY.physicalObjects) appState

    sequence_ (fmap renderPhysicalObject pos)

    SVI.present renderer


renderPhysicalObject :: PPT.PhysicalObject -> CTY.Game ()
renderPhysicalObject po = do
    res <- ask
    let pos = PPT.unPosition $ CLE.view PPT.pos po
        window = CLE.view CTY.window res
        renderer = CLE.view CTY.renderer res
    windowsize <- SDL.get $ SVI.windowSize window

    -- TODO: Fix this with actual physical size
    let coord = (fmap truncate) (pos * (fmap fromIntegral windowsize))

    SVI.rendererDrawColor renderer SDL.$= SVE.V4 255 0 0 0
    let size = fmap FCT.CInt (SVE.V2 10 10)
        p    = fmap FCT.CInt coord
    SVI.fillRect renderer (Just (SVI.Rectangle (SVE.P p) size))

wrap :: Float -> Float -> Float
wrap bound x
    | x < 0     = wrap bound (x + bound)
    | x > bound = wrap bound (x - bound)
    | otherwise = x

wrapTorus :: PPT.PhysicalObject -> PPT.PhysicalObject
wrapTorus = CLE.over PPT.pos cutOffP
  where
    PPT.PhysicalSize (SVE.V2 xMax yMax) = PPC.physicalSize
    cutOff :: SVE.V2 Float -> SVE.V2 Float
    cutOff (SVE.V2 x y) = SVE.V2 (wrap xMax x) (wrap yMax y)
    cutOffP :: PPT.Position -> PPT.Position
    cutOffP = PPT.Position . cutOff . PPT.unPosition

advanceGameState :: CTY.Game ()
advanceGameState = do
  appState <- get
  let dt = CLE.view (CTY.game . CTY.dt) appState
      f  =  (fmap wrapTorus)
            . (fmap (PPA.integrateObject dt)) 
            . (PPA.calculateGravitationalForces PPC.g)
            . PPA.resetForces
  modify (CLE.over (CTY.game . CTY.physicalObjects) f)
