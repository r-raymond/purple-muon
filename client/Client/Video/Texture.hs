module Client.Video.Texture
    ( newTextureLoader
    , loadTextureAtlas
    , loadTexture
    , renderTexture
    ) where

import           Protolude

import qualified Codec.Picture        as CPI
import qualified Data.Vector.Storable as DVS
import qualified SDL.Video.Renderer   as SVR

import qualified Client.Video.Types   as CVT

newTextureLoader :: CVT.TextureLoader
newTextureLoader = undefined


loadTextureAtlas :: FilePath -> CVT.TextureLoader -> CVT.TextureLoader
loadTextureAtlas _ _ = undefined


loadTexture :: CVT.TextureLoader -> FilePath -> Maybe CVT.TexUUID
loadTexture = undefined


renderTexture :: MonadIO m
              => CVT.TextureLoader
              -> CVT.TexUUID
              -> CVT.Position
              -> m ()
renderTexture _ _ _ = do
    _ <- liftIO $ readFile "Whaat"
    return ()


loadSurface :: MonadIO m => FilePath -> m (SVR.Surface)
loadSurface p = do
    img <- CPI.readImage p
