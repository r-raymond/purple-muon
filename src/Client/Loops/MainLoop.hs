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

module Client.Loops.MainLoop
    ( loop
    ) where

import           Protolude

import           Version

import qualified Control.Lens            as CLE
import qualified Control.Lens.Zoom       as CLZ
import qualified SDL

import qualified Client.Frames           as CFR
import qualified Client.Loops.InGameLoop as CLI
import qualified Client.Loops.MenuLoop   as CLM
import qualified Client.Types            as CTY

loop :: CTY.Game ()
loop = do
    res <- ask
    sta <- get
    let renderer = CLE.view CTY.renderer res

    --
    -- FRAME BEGIN
    --

    SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 0
    SDL.clear renderer

    CLZ.zoom (CTY.frameState . CTY.frameBegin) CFR.frameBegin

    let st = CLE.view CTY.game sta

    case st of
        CTY.IGS _ -> CLE.zoom (CTY.game . CTY.igs) CLI.loop
        CTY.MS  _ -> CLE.zoom (CTY.game . CTY.ms)  CLM.loop


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
