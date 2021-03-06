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

{-|
Module      : Client.States.InGameState.Loop
Description : The loop function while in the game
Copyright   : (c) Robin Raymond, 2016-2017
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}

module Client.States.InGameState.Loop
    ( loop
    ) where

import           Protolude

import qualified Client.States.InGameState.Types as CSIT
import qualified Client.Types                    as CTY

-- | The loop while being in game
-- Note that loop should _not_ pass any errors further along, unless they are
-- fatal. There is no recovery beyond this point, hence no MonadError.
loop :: (MonadIO m, MonadState CSIT.State m, MonadReader CTY.Resources m)
     => m ()
loop = return ()




--import qualified Control.Concurrent.STM   as CCS
--import qualified Control.Lens             as CLE
--import qualified Control.Lens.Zoom        as CLZ
--import qualified Data.IntMap.Strict       as DIS
--import qualified Formatting               as FOR
--import qualified SDL                      as SDL
--import qualified SDL.Event                as SEV
--import qualified SDL.Font                 as SFO
--import qualified SDL.Mixer                as SMI
--import qualified SDL.Vect                 as SVE
--import qualified SDL.Video                as SVI
--
--import qualified PurpleMuon.Game.Types    as PGT
--import qualified PurpleMuon.Input.Util    as PIU
--import qualified PurpleMuon.Network.Types as PNT
--import qualified PurpleMuon.Types         as PTY
--
--import qualified Client.Assets.Font       as CAF
--import qualified Client.Assets.Generic    as CAG
--import qualified Client.Assets.Sound      as CAS
--import qualified Client.Assets.Util       as CAU
--import qualified Client.Event             as CEV
--import qualified Client.Frames            as CTF
--import qualified Client.Types             as CTY
--import qualified Client.Video.Sprite      as CVS
--


--playBackgroundMusic :: MonadIO m => m ()
--playBackgroundMusic = do
--    let callback f p = putStrLn (FOR.format (FOR.fixed 0 FOR.% "%: loading " FOR.% FOR.stext) f p)
--    sl <- CAS.soundLoader
--    Right () <- runExceptT $ CAG.loadAssets_ sl CAU.soundAssets callback
--    (Right a) <- runExceptT $ CAG.getAsset sl (CAG.AssetID "click1")
--    SMI.playForever a
--
--loadFonts :: MonadIO m => CAF.FontLoaderType -> m ()
--loadFonts fl = do
--    let callback f p = putStrLn (FOR.format (FOR.fixed 0 FOR.% "%: loading " FOR.% FOR.stext) f p)
--    res <- runExceptT $ CAG.loadAssets fl (CAF.FontSize 24) CAU.fontAssets callback
--    case res of
--        Left e   -> print e
--        Right () -> return ()
--
--initLoop :: CTY.Game()
--initLoop = do
--    sta <- get
--    let sl = CLE.view CTY.sprites sta
--        callback f p = putStrLn (FOR.format (FOR.fixed 0 FOR.% "%: loading " FOR.% FOR.stext) f p)
--        fl = CLE.view CTY.fonts sta
--
--
--    playBackgroundMusic
--    loadFonts fl
--    res <- runExceptT $ CAG.loadAssets_ sl CAU.pngAssets callback
--    case res of
--        Right () -> loop
--        Left e   -> panic $ "Could not load assets: " <> e
--
--
--loop :: CTY.Game ()
--loop = do
--    CLZ.zoom (CTY.frameState . CTY.frameBegin) CTF.frameBegin
--
--    network
--
--    res <- ask
--    let window   = CLE.view CTY.window   res
--
--    SEV.mapEvents CEV.handleEvent
--    render
--
--    -- Update keyboard state
--    st <- get
--    let km = CTY._keymap $ CTY._game  st
--    CLZ.zoom (CTY.game . CTY.controls) $
--        PIU.updateKeyboardState km
--
--
--    -- advanceGameState
--
--    CLZ.zoom CTY.frameState CTF.manageFps
--
--    sta <- get
--    let fpsC = CLE.view (CTY.frameState . CTY.fpsCounter) sta
--        fps = CTF.formatFps fpsC
--    SVI.windowTitle window SDL.$= ("PM " <> gitTag <> " (" <> fps <> ")")
--    whenM (fmap (CLE.view CTY.running) get) loop
--
--render :: CTY.Game ()
--render = do
--    res <- ask
--    sta <- get
--    let renderer = CLE.view CTY.renderer res
--        sl       = CLE.view CTY.sprites sta
--    SVI.rendererDrawColor renderer SDL.$= SVE.V4 0 0 0 0
--    SVI.clear renderer
--
--    void $ runExceptT $ CVS.renderSprite renderer sl (CAG.AssetID "background.png") Nothing 0
--                        CVS.noFlip
--
--    appState <- get
--    let pos = CLE.view (CTY.game . CTY.physicalObjects) appState
--        gos = CLE.view (CTY.game . CTY.gameObjects) appState
--        ngos = fmap (CVS.updateRenderInfo pos) gos
--
--    void $ runExceptT $ sequence_ (fmap (CVS.renderGameObject renderer
--                                          sl
--                                          (SDL.V2 640 480) -- TODO : fix
--                                          ) ngos)
--
--    -- Test rendering of font
--    let fl = CLE.view CTY.fonts sta
--    Right f <- runExceptT $ CAG.getAsset fl (CAG.AssetID "kenvector_future_thin:24")
--    sur <- SFO.blended f (SDL.V4 255 255 255 0) "Hello World"
--    t <- SDL.createTextureFromSurface renderer sur
--    SDL.copy renderer t Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 0) (SDL.V2 200 50))
--
--
--    SVI.present renderer
--
--
---- TODO: Move this in own module
--network :: CTY.Game ()
--network = do
--    sta <- get
--    let s = CTY._tbqueue $ CTY._netState $ CTY._game sta
--    bin <- liftIO $ CCS.atomically $ CCS.tryReadTBQueue s
--    case bin of
--        Just (PNT.Update objs) -> do
--            modify (CLE.set (CTY.game . CTY.physicalObjects) objs)
--            network
--        Just (PNT.Ping) -> network                  -- < TODO
--        Just (PNT.CreateGameObject (k, o, mp)) ->
--            let key = PTY.unKey k
--            in case mp of
--                Nothing -> do
--                    modify (CLE.over (CTY.game . CTY.gameObjects) (DIS.insert key o))
--                    network
--                Just p -> do
--                    let mpk = fmap PTY.unKey (CLE.view PGT.mPhOb o)
--                    case mpk of
--                        Just pk -> do
--                            modify (CLE.over (CTY.game . CTY.physicalObjects) (DIS.insert pk p))
--                            modify (CLE.over (CTY.game . CTY.gameObjects) (DIS.insert key o))
--                            network
--                        Nothing -> network -- TODO: Log error. got physical object but no matching id
--        Nothing -> return ()
