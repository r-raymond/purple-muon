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

module Client.Assets.Texture
    ( textureLoader
    , TextureLoaderType
    , TextureID
    ) where

import           Protolude

import qualified Data.HashTable.IO          as DHI
import qualified SDL
import qualified SDL.Image                  as SIM
import qualified System.FilePath.Posix      as SFP

import qualified Client.Assets.Generic      as CAG
import qualified PurpleMuon.Util.MonadError as PUM

-- | Type of a texture loader
type TextureLoaderType = CAG.HashmapLoader SDL.Texture SDL.Renderer

-- | Type of a texture identifier
type TextureID = CAG.AssetID (CAG.Asset TextureLoaderType)

-- | Implementation of `AssetLoader` for textures
textureLoader :: MonadIO m => SDL.Renderer -> m TextureLoaderType
textureLoader ren = do
    ht <- liftIO $ DHI.new
    return (CAG.HashmapLoader
        { CAG.store = ht
        , CAG.extraData = ren
        , CAG.loadFromFile = \r p -> do
                        t <- try $ SIM.loadTexture r p
                                :: IO (Either SomeException SDL.Texture)
                        let res = PUM.mapLeft show t
                            i = CAG.AssetID $ toS $ SFP.takeBaseName p
                        return (fmap (\x -> [(i, x)]) res)
        , CAG.delete = \_ t -> SDL.destroyTexture t
        })

