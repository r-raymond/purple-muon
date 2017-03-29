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
Module      : Client.State.MenuState.Init
Description : Create a new menu state
Copyright   : (c) Robin Raymond, 2016-2017
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}

module Client.States.MenuState.Init
    ( initMenu
    ) where

import           Protolude

import           Version

import qualified SDL

import qualified Client.Assets.Font            as CAF
import qualified Client.Assets.Generic         as CAG
import qualified Client.Assets.Sprite          as CAS
import qualified Client.Assets.Texture         as CAT
import qualified Client.States.MenuState.Types as CSMT
import qualified Client.States.Types           as CST
import qualified Client.Video.Menu             as CVM
import qualified PurpleMuon.Types              as PTY

-- | initialize a new menu
initMenu :: MonadIO m => SDL.Renderer -> m CST.State
initMenu r = do
    tl <- CAT.textureLoader r
    sl <- CAS.spriteLoader tl
    fl <- CAF.fontLoader
    res <- runExceptT $ do
            CAG.loadAssets fl menuFonts (\_ _ -> return ())
            mainMenu sl fl r
    case res of
        Right mm -> return $ CST.MenuState $ CSMT.State sl mm fl
        Left e   -> panic e

-- | list of fonts used within the menu
menuFonts :: [(CAF.FontSize, FilePath)]
menuFonts = fmap (\(x,y) -> (CAF.FontSize x, "res/fonts/" ++ y))
    [ (48, "kenvector_future_thin.ttf") ]

mainMenu :: (MonadIO m, MonadError Text m)
         => CAS.SpriteLoaderType
         -> CAF.FontLoaderType
         -> SDL.Renderer
         -> m [CVM.MenuItem]
mainMenu sl fl r = do
    let headerC = PTY.Color $ SDL.V4 137 189 211 0
    let versionC = PTY.Color $ SDL.V4 255 255 255 0
    f <- CAG.getAsset fl (CAG.AssetID "kenvector_future_thin:48")
    header <- CVM.mkLabel sl r headerC
                    (PTY.Position $ SDL.V2 0.1 0.1)
                    (PTY.Size $ SDL.V2 0.6 0.2)
                    "Purple Muon" f
    version <- CVM.mkLabel sl r versionC
                    (PTY.Position $ SDL.V2 0.5 0.22)
                    (PTY.Size $ SDL.V2 0.1 0.05)
                    ("v." <> gitTag) f

    return [header, version]

