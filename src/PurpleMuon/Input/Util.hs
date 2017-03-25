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
Module      : PurpleMuon.Input.Util
Description : Some utility functions for input management
Copyright   : (c) Robin Raymond, 2016-2017
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}

module PurpleMuon.Input.Util
    ( updateKeyboardState
    , getKeyboardState
    , standardKeyMap
    ) where

import Protolude

import qualified SDL

import PurpleMuon.Input.Types as PIT

-- | Update the current state of the keyboard
updateKeyboardState :: (MonadIO m, MonadState PIT.Controls m)
                    => PIT.KeyMap
                    -> m ()
updateKeyboardState km = getKeyboardState km >>= put

-- | Get the current state of the keyboard
getKeyboardState :: MonadIO m
                 => PIT.KeyMap
                 -> m PIT.Controls
getKeyboardState (PIT.KeyMap a b c d e f g h) = do
    ks <- SDL.getKeyboardState
    let [i,j,k,l,m,n,o,p] = fmap ks [a,b,c,d,e,f,g,h]
    return $ PIT.Controls i j k l m n o p

-- | Standard keymap
standardKeyMap :: PIT.KeyMap
standardKeyMap
    = PIT.KeyMap
    { PIT.km_accel = SDL.ScancodeUp
    , PIT.km_turn_left = SDL.ScancodeLeft
    , PIT.km_turn_right = SDL.ScancodeRight
    , PIT.km_decel = SDL.ScancodeDown
    , PIT.km_fire1 = SDL.ScancodeSpace
    , PIT.km_fire2 = SDL.ScancodeS
    , PIT.km_fire3 = SDL.ScancodeD
    , PIT.km_fire4 = SDL.ScancodeF
    }
