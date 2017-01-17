--  Copyright 2016, 2017 Robin Raymond
--
--  This file is part of Purple Muon
--
--  Purple Muon is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  Purple Muon is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Purple Muon.  If not, see <http://www.gnu.org/licenses/>.

module Client.MainLoop
    ( loop
    , initLoop
    ) where

import           Protolude

import           Paths_purple_muon
import           Version

import qualified Control.Concurrent.STM   as CCS
import qualified Control.Lens             as CLE
import qualified Data.Binary              as DBI
import qualified Data.IntMap.Strict       as DIS
import qualified Foreign.C.Types          as FCT
import qualified SDL                      as SDL
import qualified SDL.Event                as SEV
import qualified SDL.Vect                 as SVE
import qualified SDL.Video                as SVI

--import qualified PurpleMuon.Physics.Algorithm as PPA
--import qualified PurpleMuon.Physics.Constants as PPC
import qualified PurpleMuon.Network.Types as PNT
import qualified PurpleMuon.Physics.Types as PPT

import qualified Client.Event             as CEV
import qualified Client.Frames            as CTF
import qualified Client.Types             as CTY
import qualified Client.Video.Texture     as CVT
import qualified Client.Video.Types       as CVTY

initLoop :: CTY.Game()
initLoop = do
    sta <- get
    gravity <- liftIO $ getDataFileName "res/png/gravity.xml"
    space   <- liftIO $ getDataFileName "res/png/space.xml"
    let tl = CLE.view CTY.textures sta
        f = texLoadHelper gravity
            >=> texLoadHelper space
    newTl <- f tl
    modify (CLE.set CTY.textures newTl)

    let (Just s) = CVT.getTexture newTl "meteorBrown_big1.png"
        (Just b) = CVT.getTexture newTl "background.png"
    loop (CTY.TextureUUIDs b s)

texLoadHelper :: MonadIO m => FilePath -> CVTY.TextureLoader -> m CVTY.TextureLoader
texLoadHelper p l = do
    nl <- runExceptT $ CVT.addTextureAtlas l p
    case nl of
        Right t -> return t
        Left e  -> panic $ "Could not load " <> (toS p) <> "\n" <> e

loop :: CTY.TextureUUIDs -> CTY.Game ()
loop tuu = do
    CTF.frameBegin

    network

    res <- ask
    let window   = CLE.view CTY.window   res

    SEV.mapEvents CEV.handleEvent
    render tuu

    -- advanceGameState

    CTF.manageFps

    fps <- CTF.formatFps
    SVI.windowTitle window SDL.$= ("PM " <> gitTag <> " (" <> fps <> ")")
    whenM (fmap (CLE.view CTY.running) get) (loop tuu)

render :: CTY.TextureUUIDs -> CTY.Game ()
render tuu = do
    res <- ask
    sta <- get
    let renderer = CLE.view CTY.renderer res
        texload  = CLE.view CTY.textures sta
    SVI.rendererDrawColor renderer SDL.$= SVE.V4 0 0 0 0
    SVI.clear renderer

    CVT.renderTexture texload (CTY.background tuu) Nothing

    appState <- get
    let pos = CLE.view (CTY.game . CTY.physicalObjects) appState

    sequence_ (fmap (renderPhysicalObject (CTY.stones tuu)) pos)

    SVI.present renderer


renderPhysicalObject :: CVTY.TexUUID -> PPT.PhysicalObject -> CTY.Game ()
renderPhysicalObject t po = do
    res <- ask
    sta <- get
    let pos = PPT.unPosition $ CLE.view PPT.pos po
        window = CLE.view CTY.window res
        texload = CLE.view CTY.textures sta
    windowsize <- SDL.get $ SVI.windowSize window

    -- TODO: Fix this with actual physical size
    let coord = fmap truncate (pos * fmap fromIntegral windowsize)
        size = fmap FCT.CInt (SVE.V2 10 10)
        p    = fmap FCT.CInt coord
        bb   = Just $ SVI.Rectangle (SVE.P p) size

    CVT.renderTexture texload t bb


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
