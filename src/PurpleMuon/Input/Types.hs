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
Module      : PurpleMuon.Input.Types
Description : Types for controls.
Copyright   : (c) Robin Raymond, 2016-2017
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX

Purple Moun Clients have `8` possible keys they can press. These are

 * accelerate (up key)
 * turn left (left key)
 * turn right (right key)
 * decelerate (down key)
 * fire 1 (space)
 * fire 2 (s)
 * fire 3 (d)
 * fire 4 (f)

These controls are send from the clients to the server 30 times a second. They
packed into a Word8, where the bits signalize pressed (1) or not pressed (0) in
the order mentioned above.
-}

module PurpleMuon.Input.Types
    ( Controls(..)
    , KeyMap(..)
    , packControls
    , unpackControls
    ) where

import           Protolude

import qualified SDL

-- | A data type that stores information about the current keyboard state
data Controls
    = Controls
    { accelerate :: Bool
    , turn_left  :: Bool
    , turn_right :: Bool
    , decelerate :: Bool
    , fire1      :: Bool
    , fire2      :: Bool
    , fire3      :: Bool
    , fire4      :: Bool
    } deriving (Show, Eq)

-- | A data type that stores a keymap. A keymap is a mapping from SDL scancodes
-- to controls defined in the game
data KeyMap
    = KeyMap
    { km_accel      :: SDL.Scancode
    , km_turn_left  :: SDL.Scancode
    , km_turn_right :: SDL.Scancode
    , km_decel      :: SDL.Scancode
    , km_fire1      :: SDL.Scancode
    , km_fire2      :: SDL.Scancode
    , km_fire3      :: SDL.Scancode
    , km_fire4      :: SDL.Scancode
    } deriving (Show, Eq)

packControls :: Controls -> Word8
packControls (Controls a b c d e f g h)
    = toWord a 1
  .|. toWord b 2
  .|. toWord c 4
  .|. toWord d 8
  .|. toWord e 16
  .|. toWord f 32
  .|. toWord g 64
  .|. toWord h 128
      where
        toWord True  n = n
        toWord False _ = 0

unpackControls :: Word8 -> Controls
unpackControls w =
    Controls (test 1) (test 2) (test 4) (test 8)
             (test 16) (test 32) (test 64) (test 128)
      where
        test x = (w .&. x) /= 0
