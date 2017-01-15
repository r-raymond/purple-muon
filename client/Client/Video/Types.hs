module Client.Video.Types
    ( TextureAtlas(..)
    , Texture(..)
    , TextureLoader(..)
    , TexUUID(..)
    , Position
    , AtlasUUID
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

type Position = (Int, Int)
