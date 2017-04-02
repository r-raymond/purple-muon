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
Module      : Client.States.Types
Description : Abstraction over states the client can be in.
Copyright   : (c) Robin Raymond, 2016-2017
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}

{-# LANGUAGE TemplateHaskell #-}

module Client.States.Types
    ( State(..), inGameState, menuState
    , CommonState(..), resolution, events
    ) where

import qualified Control.Lens                    as CLE
import qualified SDL

import qualified Client.States.InGameState.Types as CSIT
import qualified Client.States.MenuState.Types   as CSMT
import qualified Client.Video.Types              as CVT

-- Info that is used by all states
data CommonState
    = CommonState
    { _resolution :: CVT.Resolution     -- ^ The current resolution of the screen
    , _events     :: [SDL.Event]        -- ^ Events to be handled by the state
    }

data State
    = InGameState
    { _inGameState :: CSIT.State
    }
    | MenuState
    { _menuState :: CSMT.State
    }

CLE.makeLenses ''State
CLE.makeLenses ''CommonState
