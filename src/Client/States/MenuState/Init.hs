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
            CAG.loadAssets_ sl menuImages (\_ _ -> return ())
            mainMenu sl fl r
    case res of
        Right mm -> return $ CST.MenuState $ CSMT.State sl mm fl
        Left e   -> panic e

-- | list of fonts used within the menu
menuFonts :: [(CAF.FontSize, FilePath)]
menuFonts = fmap (\(x,y) -> (CAF.FontSize x, "res/fonts/" ++ y))
    [ (48, "fyodor/truetype/Fyodor-BoldExpandedOblique.ttf")
    , (48, "fyodor/truetype/Fyodor-BoldExpanded.ttf") ]

-- | list of all images used in the menu
menuImages :: [FilePath]
menuImages = fmap (\x -> "res/png/" ++ x)
    [ "logo.xml"
    , "uipackSpace_sheet.xml"
    ]


mainMenu :: (MonadIO m, MonadError Text m)
         => CAS.SpriteLoaderType
         -> CAF.FontLoaderType
         -> SDL.Renderer
         -> m [CVM.MenuItem]
mainMenu sl fl r = do
    let headerC = PTY.Color $ SDL.V4 104 178 248 0
        versionC = PTY.Color $ SDL.V4 255 255 255 0
        logo = CVM.mkImage (CAG.AssetID "logo.png")
                           (PTY.Position $ SDL.V2 0.1 0.1)
                           (PTY.Size $ SDL.V2 0.2 0.2)
        button1 = CVM.mkImage (CAG.AssetID "glassPanel_corners.png")
                            (PTY.Position $ SDL.V2 0.3 0.4)
                            (PTY.Size $ SDL.V2 0.4 0.2)
        button2 = CVM.mkImage (CAG.AssetID "glassPanel_corners.png")
                            (PTY.Position $ SDL.V2 0.3 0.65)
                            (PTY.Size $ SDL.V2 0.4 0.2)

    f <- CAG.getAsset fl (CAG.AssetID "Fyodor-BoldExpandedOblique:48")
    header <- CVM.mkLabel sl r headerC
                    (PTY.Position $ SDL.V2 0.35 0.11)
                    (PTY.Size $ SDL.V2 0.6 0.1)
                    "Purple Muon" f
    version <- CVM.mkLabel sl r versionC
                    (PTY.Position $ SDL.V2 0.6 0.22)
                    (PTY.Size $ SDL.V2 0.2 0.05)
                    ("v." <> gitTag) f
    bl1 <- CVM.mkLabel sl r headerC
                    (PTY.Position $ SDL.V2 0.35 0.45)
                    (PTY.Size $ SDL.V2 0.3 0.1)
                    "New Game" f
    bl2 <- CVM.mkLabel sl r headerC
                    (PTY.Position $ SDL.V2 0.35 0.7)
                    (PTY.Size $ SDL.V2 0.3 0.1)
                    "Exit" f



    return [header, version, logo, button1, button2, bl1, bl2]

