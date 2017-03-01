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

import           Version

import qualified Control.Concurrent.STM   as CCS
import qualified Control.Lens             as CLE
import qualified SDL                      as SDL
import qualified SDL.Event                as SEV
import qualified SDL.Vect                 as SVE
import qualified SDL.Video                as SVI

import qualified PurpleMuon.Network.Types as PNT

import qualified Client.Assets.Util       as CAU
import qualified Client.Event             as CEV
import qualified Client.Frames            as CTF
import qualified Client.Types             as CTY
import qualified Client.Video.Render      as CVR
import qualified Client.Video.Texture     as CVT

initLoop :: CTY.Game()
initLoop = do
    res <- ask
    let ren = CLE.view CTY.renderer res

    etl <- runExceptT $ CAU.loadAllPngAssets ren
    case etl of
        Right tl -> do
            modify (CLE.set CTY.textures tl)

            let (Just s) = CVT.getTexture tl "meteorBrown_big1.png"
                (Just b) = CVT.getTexture tl "background.png"
            loop (CTY.TextureUUIDs b s)
        Left e -> panic $ "Could not load assets: " <> e

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

    sequence_ (fmap (CVR.renderGameObjects (CTY.stones tuu)) pos)

    SVI.present renderer



network :: CTY.Game ()
network = do
    res <- ask
    let s = CLE.view CTY.tbqueue res
    bin <- liftIO $ CCS.atomically $ CCS.tryReadTBQueue s
    case bin of
        Just (PNT.Update objs) -> do
            modify (CLE.set (CTY.game . CTY.physicalObjects) objs)
            network
        Just (PNT.Ping) -> network                  -- < TODO
        Just (PNT.CreateGameObject _) -> network    -- < TODO
        Nothing -> return ()
