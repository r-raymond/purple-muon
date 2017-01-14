module Client.Video.Types
    ( TextureAtlas(..)
    , Texture(..)
    , TextureLoader(..)
    , TexUUID(..)
    , Position
    ) where


import Protolude

import qualified SDL.Video.Renderer as SVR
import qualified Data.IntMap.Strict as DIS

newtype TextureAtlas = TextureAtlas { unTextureAtlas :: SVR.Surface }

data Texture
    = Texture
    { atlas  :: Int
    , name   :: Text
    , x      :: Int
    , y      :: Int
    , width  :: Int
    , height :: Int
    }

data TextureLoader
    = TextureLoader
    { atlases   :: DIS.IntMap TextureAtlas
    , textures  :: DIS.IntMap Texture
    }

newtype TexUUID = TexUUID { unTexUUID :: Int }

type Position = (Int, Int)
