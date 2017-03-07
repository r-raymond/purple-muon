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
    , loadAllPngAssets
    ) where

import           Protolude

import           Paths_purple_muon

import qualified SDL as SDL
import qualified System.FilePath.Posix as SFP

import qualified Client.Video.Texture as CVT
import qualified Client.Video.Types    as CVTY

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

-- |Load all png assets into new texture loader
loadAllPngAssets :: (MonadError Text m, MonadIO m)
                 => SDL.Renderer             -- ^ Renderer to use to upload
                                             -- textures to the video card.
                 -> (Float -> Text -> m ())  -- ^ Callback function (see
                                             -- `loadPngAssets`)
                 -> m CVTY.TextureLoader
loadAllPngAssets r c = loadPngAssets (CVT.newTextureLoader r) pngAssets c

-- |Load png assets.
loadPngAssets :: forall m. (MonadError Text m, MonadIO m)
              => CVTY.TextureLoader         -- ^ Textureloader to load textures
                                            -- into
              -> [FilePath]                 -- ^ Paths to xml files. Note that
                                            -- these should be specified
                                            -- relative to the root of the git
                                            -- directory
              -> (Float -> Text -> m ())    -- ^ Callback function. This
                                            -- function will be called whenever
                                            -- a new texture is loading with the
                                            -- percentage of loaded files and
                                            -- the name of the currently loading
                                            -- texture. If this is not needed,
                                            -- set it to `return ()`.
              -> m CVTY.TextureLoader
loadPngAssets tl paths callback = do
    -- convert names to file paths
    ps <- liftIO $ sequence (fmap getDataFileName paths)
    -- pair with percentages
    let perc = fmap (\x -> 100 * x / (fromIntegral $ length ps)) [1 ..]
        pspe = zip ps perc
    -- functions for loading assets
        loadAsset :: (FilePath, Float) -> CVTY.TextureLoader -> m CVTY.TextureLoader
        loadAsset = \(p, per) tlo -> do
            callback per (toS $ SFP.takeFileName p)
            CVT.addTextureAtlas tlo p 
        loadAll = fmap loadAsset pspe
    foldl' (>>=) (return tl) loadAll
