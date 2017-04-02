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
    ( handleCommonEvent
    , isCommonEvent
    ) where

import           Protolude

import qualified Control.Lens        as CLE
import qualified SDL
import qualified SDL.Input.Keyboard  as SIK

import qualified Client.States.Types as CST
import qualified Client.Types        as CTY
import qualified Client.Video.Types  as CVT

-- Is this a common event
--
-- A common event is an event that will be handled before passing it to the
-- application states
isCommonEvent :: SDL.Event -> Bool
isCommonEvent (SDL.Event _ (SDL.WindowResizedEvent _)) = True
isCommonEvent _                                        = False

-- Handle a common event
handleCommonEvent :: SDL.Event -> CTY.Game ()
handleCommonEvent ev = case (SDL.eventPayload ev) of
    SDL.WindowResizedEvent e -> modify $ CLE.set (CTY.comState . CST.resolution)
        (CVT.Resolution $ SDL.windowResizedEventSize e)
    _                   -> return ()
