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

{-# LANGUAGE TemplateHaskell #-}

module Client.Loop
   ( loop
   ) where

import           Protolude

import           Version

import qualified Control.Lens                   as CLE
import qualified Control.Lens.Zoom              as CLZ
import qualified Data.List                      as DLI
import qualified SDL

import qualified Client.Event                   as CEV
import qualified Client.Frames                  as CFR
import qualified Client.States.InGameState.Loop as CSIL
import qualified Client.States.MenuState.Loop   as CSML
import qualified Client.States.Types            as CST
import qualified Client.Types                   as CTY

loop :: CTY.Game ()
loop = do
    res <- ask
    sta <- get
    let renderer = CLE.view CTY.renderer res

    --
    -- FRAME BEGIN
    --

    -- Input handling
    events <- SDL.pollEvents
    let (commEv, stateEv) = DLI.partition CEV.isCommonEvent events

    -- Set the events for the state
    modify $ CLE.set (CTY.comState . CST.events) stateEv

    -- handle common events
    sequence_ $ fmap CEV.handleCommonEvent commEv


    SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 0
    SDL.clear renderer

    CLZ.zoom (CTY.frameState . CTY.frameBegin) CFR.frameBegin

    let st = CLE.view CTY.game sta
        cs = CLE.view CTY.comState sta

    case st of
        CST.InGameState _ -> CLE.zoom (CTY.game . CST.inGameState) CSIL.loop
        CST.MenuState   _ -> CLE.zoom (CTY.game . CST.menuState)   (CSML.loop cs)


    CLZ.zoom CTY.frameState CFR.manageFps


    SDL.present renderer

    --
    -- FRAME END
    --

    let fpsC = CLE.view (CTY.frameState . CTY.fpsCounter) sta
        fps = CFR.formatFps fpsC
        window = CLE.view CTY.window res
    SDL.windowTitle window SDL.$= ("PM " <> gitTag <> " (" <> fps <> ")")
    whenM (fmap (CLE.view CTY.running) get) loop
