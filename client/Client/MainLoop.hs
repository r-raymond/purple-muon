module Client.MainLoop
    ( loop
    , initLoop
    ) where

import           Protolude

import qualified Control.Concurrent.STM    as CCS
import qualified Control.Lens              as CLE
import qualified Data.Binary               as DBI
import qualified Data.IntMap.Strict        as DIS
import qualified Foreign.C.Types           as FCT
import qualified SDL                       as SDL
import qualified SDL.Event                 as SEV
import qualified SDL.Vect                  as SVE
import qualified SDL.Video                 as SVI
import qualified SDL.Video.Renderer        as SVR

--import qualified PurpleMuon.Physics.Algorithm as PPA
--import qualified PurpleMuon.Physics.Constants as PPC
import qualified PurpleMuon.Network.Types  as PNT
import qualified PurpleMuon.Physics.Types  as PPT

import qualified Client.Event              as CEV
import qualified Client.Frames             as CTF
import qualified Client.Types              as CTY
import qualified Client.Video.Texture      as CVT

initLoop :: CTY.Game()
initLoop = do
    (Right s) <- runExceptT $ CVT.loadSurface "res/png/space.png"
    res <- ask
    let renderer = CLE.view CTY.renderer res
    t <- SVR.createTextureFromSurface renderer s
    loop t


loop :: SVR.Texture -> CTY.Game ()
loop back = do
    CTF.frameBegin

    network

    res <- ask
    let window   = CLE.view CTY.window   res

    SEV.mapEvents CEV.handleEvent
    render back

    -- advanceGameState

    CTF.manageFps

    fps <- CTF.formatFps
    SVI.windowTitle window SDL.$= fps
    whenM (fmap (CLE.view CTY.running) get) (loop back)

render :: SVR.Texture -> CTY.Game ()
render back = do
    res <- ask
    let renderer = CLE.view CTY.renderer res
    SVI.rendererDrawColor renderer SDL.$= SVE.V4 0 0 0 0
    SVI.clear renderer

    SVR.copy renderer back Nothing Nothing

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
    let coord = fmap truncate (pos * fmap fromIntegral windowsize)

    SVI.rendererDrawColor renderer SDL.$= SVE.V4 255 0 0 0
    let size = fmap FCT.CInt (SVE.V2 10 10)
        p    = fmap FCT.CInt coord
    SVI.fillRect renderer (Just (SVI.Rectangle (SVE.P p) size))

--advanceGameState :: CTY.Game ()
--advanceGameState = do
--  appState <- get
--  let dt = CLE.view (CTY.game . CTY.dt) appState
--  modify (CLE.over (CTY.game . CTY.physicalObjects)
--                   (\x -> (PPA.integrateTimeStep PPC.g dt x DIS.empty)))

network :: CTY.Game ()
network = do
    res <- ask
    let s = CLE.view CTY.tbqueue res
    bin <- liftIO $ CCS.atomically $ CCS.tryReadTBQueue s
    case bin of
        Just n -> do
            let objs = DBI.decode $ toS $ PNT.unNakedMessage n :: [(Int, PPT.PhysicalObject)]
            modify (CLE.set (CTY.game . CTY.physicalObjects) (DIS.fromList objs))
            network
        Nothing -> return ()
