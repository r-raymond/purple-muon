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
Module      : Client.States.MenuState.Loop
Description : The loop while being in the menu
Copyright   : (c) Robin Raymond, 2016-2017
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}

module Client.States.MenuState.Loop
    ( loop
    ) where

import           Protolude

import qualified Control.Lens                  as CLE
import qualified SDL

import qualified Client.States.MenuState.Types as CSMT
import qualified Client.States.Types           as CST
import qualified Client.Types                  as CTY
import qualified Client.Video.Menu             as CVM

-- | The loop while being in the menu.
-- Note that loop should _not_ pass any errors further along, unless they are
-- fatal. There is no recovery beyond this point, hence no MonadError.
loop :: (MonadIO m, MonadState CSMT.State m, MonadReader CTY.Resources m)
     => CST.CommonState
     -> m ()
loop cs = do
    sta <- get
    res <- ask
    let sl = CLE.view CSMT.menuSprites sta
        ren = CLE.view CTY.renderer res
        resolution = CLE.view CST.resolution cs

    SDL.rendererDrawColor ren SDL.$= SDL.V4 0x2e 0x34 0x36 0
    SDL.clear ren


    r <- runExceptT $ sequence_ $
        fmap (CVM.renderMenuItem resolution sl ren) (CLE.view CSMT.menuItems sta)
    return ()
