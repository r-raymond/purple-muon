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
Module      : Client.States.MenuState.Types
Description : The types used for the menu state.
Copyright   : (c) Robin Raymond, 2016-2017
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}

{-# LANGUAGE TemplateHaskell #-}

module Client.States.MenuState.Types
    ( State(..), menuSprites, menuItems, menuFonts
    ) where

import qualified Control.Lens         as CLE

import qualified Client.Assets.Font   as CAF
import qualified Client.Assets.Sprite as CAS
import qualified Client.Video.Menu    as CVM

data State
    = State
    { _menuSprites :: CAS.SpriteLoaderType
    , _menuItems   :: [CVM.MenuItem]
    , _menuFonts   :: CAF.FontLoaderType
    }

CLE.makeLenses ''State
