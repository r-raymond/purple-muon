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

module Client.Video.Types
    ( TextureAtlas(..)
    , Texture(..)
    , TextureLoader(..)
    , TexUUID(..)
    , Position
    , AtlasUUID
    , RenderTexture(..)
    ) where


import Protolude

import qualified SDL.Video.Renderer as SVR
import qualified Data.IntMap.Strict as DIS
import qualified Foreign.C.Types    as FCT

newtype TextureAtlas = TextureAtlas { unTextureAtlas :: SVR.Texture }

data Texture
    = Texture
    { atlas  :: AtlasUUID
    , name   :: Text
    , rect   :: SVR.Rectangle FCT.CInt
    }

data TextureLoader
    = TextureLoader
    { atlases   :: DIS.IntMap TextureAtlas
    , textures  :: DIS.IntMap Texture
    , renderer  :: SVR.Renderer
    , nextKey   :: AtlasUUID
    }

type AtlasUUID = Int

newtype TexUUID = TexUUID { unTexUUID :: Int }

type Position = (Word16, Word16)

data RenderTexture
    = RenderTexture
    { uuid :: TexUUID
    , pos  :: Position
    }
