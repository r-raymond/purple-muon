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

module Client.Event
    ( handleEvent
    ) where

import Protolude

import qualified Control.Lens as CLE
import qualified SDL.Input.Keyboard        as SIK
import qualified SDL.Event as SEV

import qualified Client.Types as CTY

handleEvent :: SEV.Event -> CTY.Game ()
handleEvent ev = case (SEV.eventPayload ev) of
    SEV.KeyboardEvent e -> handleKeyboardEvent e
    _ -> return ()


handleKeyboardEvent :: SEV.KeyboardEventData -> CTY.Game ()
handleKeyboardEvent (SEV.KeyboardEventData _ SEV.Pressed _ (SIK.Keysym SIK.ScancodeEscape _ _)) =
    modify (CLE.set CTY.running False)
handleKeyboardEvent _ = return ()
