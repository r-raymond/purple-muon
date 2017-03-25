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
    ( getKeyboardState
    ) where

import Protolude

import qualified SDL

import PurpleMuon.Input.Types as PIT

-- | Get the current state of the keyboard
getKeyboardState :: (MonadIO m, MonadReader PIT.KeyMap m)
                 => m PIT.Controls
getKeyboardState = do
    (PIT.KeyMap a b c d e f g h) <- ask
    ks <- SDL.getKeyboardState
    let [i,j,k,l,m,n,o,p] = fmap ks [a,b,c,d,e,f,g,h]
    return $ PIT.Controls i j k l m n o p


