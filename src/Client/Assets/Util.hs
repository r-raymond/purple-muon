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


module Client.Assets.Util
    ( pngAssets
    , soundAssets
    , fontAssets
    ) where

import           Protolude

-- | A list of all png assets to load on game start.
-- If this list gets too long, maybe it should be split into assets for
-- different stages of the game.
pngAssets :: [FilePath]
pngAssets = [ "res/png/gravity.xml"
            , "res/png/space.xml"
            , "res/png/blueSheet.xml"
            , "res/png/greenSheet.xml"
            , "res/png/redSheet.xml"
            , "res/png/greySheet.xml"
            -- Segfaults: TODO: Investigate why
            --, "res/png/yellowSheet.xml"
            , "res/png/spaceShooter2_spritesheet.xml"
            , "res/png/spaceShooter2_spritesheet_2X.xml"
            , "res/png/uipackSpace_sheet.xml"
            , "res/png/gameicons/sheet_black1x.xml"
            , "res/png/gameicons/sheet_black2x.xml"
            , "res/png/gameicons/sheet_white1x.xml"
            , "res/png/gameicons/sheet_white2x.xml"
            , "res/png/gameicons_extended/sheet_black1x.xml"
            , "res/png/gameicons_extended/sheet_black2x.xml"
            , "res/png/gameicons_extended/sheet_colored1x.xml"
            , "res/png/gameicons_extended/sheet_colored2x.xml"
            , "res/png/gameicons_extended/sheet_white1x.xml"
            , "res/png/gameicons_extended/sheet_white2x.xml"
            ]

-- | Complete list of all sound assets in the game
soundAssets :: [FilePath]
soundAssets =
    [ "res/ogg/click1.ogg"
    , "res/ogg/click2.ogg"
    , "res/ogg/rollover1.ogg"
    , "res/ogg/rollover2.ogg"
    , "res/ogg/switch2.ogg"
    , "res/ogg/switch3.ogg"
    ]

-- | Complete list of all font assets used in the game
fontAssets :: [FilePath]
fontAssets =
    [ "res/fonts/kenpixel.ttf"
    , "res/fonts/kenpixel_blocks.ttf"
    , "res/fonts/kenpixel_future.ttf"
    , "res/fonts/kenpixel_future_square.ttf"
    , "res/fonts/kenpixel_high.ttf"
    , "res/fonts/kenpixel_high_square.ttf"
    , "res/fonts/kenpixel_mini.ttf"
    , "res/fonts/kenpixel_mini_square.ttf"
    , "res/fonts/kenpixel_square.ttf"
    , "res/fonts/kenvector_future.ttf"
    , "res/fonts/kenvector_future_thin.ttf"
    ]
