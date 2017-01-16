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

--import qualified PurpleMuon.Physics.Algorithm as PPA
--import qualified PurpleMuon.Physics.Constants as PPC
import qualified PurpleMuon.Network.Types  as PNT
import qualified PurpleMuon.Physics.Types  as PPT

import qualified Client.Event              as CEV
import qualified Client.Frames             as CTF
import qualified Client.Types              as CTY
import qualified Client.Video.Types        as CVTY
import qualified Client.Video.Texture      as CVT

initLoop :: CTY.Game()
initLoop = do
    res <- ask
    let renderer = CLE.view CTY.renderer res
        tl = CVT.newTextureLoader renderer
    (Right mtl) <- runExceptT $  CVT.addTextureAtlas tl "res/png/gravity.xml"
    mtl2 <- runExceptT $  CVT.addTextureAtlas mtl "res/png/space.xml"
    case mtl2 of
        Right t -> loop t
        Left er -> putStrLn er

loop :: CVTY.TextureLoader -> CTY.Game ()
loop tl = do
    CTF.frameBegin

    network

    res <- ask
    let window   = CLE.view CTY.window   res

    SEV.mapEvents CEV.handleEvent
    let (Just u) = CVT.getTexture tl "background.png"
    render tl u

    -- advanceGameState

    CTF.manageFps

    fps <- CTF.formatFps
    SVI.windowTitle window SDL.$= fps
    whenM (fmap (CLE.view CTY.running) get) (loop tl)

render :: CVTY.TextureLoader -> CVTY.TexUUID -> CTY.Game ()
render tl u = do
    res <- ask
    let renderer = CLE.view CTY.renderer res
    SVI.rendererDrawColor renderer SDL.$= SVE.V4 0 0 0 0
    SVI.clear renderer

    CVT.renderTexture tl u Nothing

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
