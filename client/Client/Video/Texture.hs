module Client.Video.Texture
    ( parseTextureAtlas
    , newTextureLoader
    , loadTextureAtlas
    , loadTexture
    , renderTexture
    , loadSurface
    ) where

import           Protolude

import qualified Codec.Picture        as CPI
import qualified Data.Vector.Storable as DVS
import qualified Foreign.C.Types      as FCT
import qualified Linear.V2            as LV2
import qualified SDL.Video.Renderer   as SVR
import qualified Text.XML.Light as TXL

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


parseTextureAtlas :: (MonadError Text m, MonadIO m) 
                  => FilePath
                  -> m TXL.Element
parseTextureAtlas p = do
    c <- liftIO $ readFile p
    let mXml = TXL.parseXMLDoc c
    case mXml of
        Nothing -> throwError ("Error parsing " <> (toS p))
        Just xm -> return xm

loadSurface :: (MonadError Text m, MonadIO m) => FilePath -> m SVR.Surface
loadSurface p = do
    mImg <- liftIO $ CPI.readImage p
    case mImg of
        Left err -> throwError $ "Could not load image: " <> (toS err)
        Right su -> loadSurfaceHelper su

loadSurfaceHelper :: MonadIO m => CPI.DynamicImage -> m SVR.Surface
loadSurfaceHelper img = do
    let rgba8  = CPI.convertRGBA8 img
        width  = CPI.imageWidth rgba8
        height = CPI.imageWidth rgba8
        dim    = fmap (FCT.CInt . fromIntegral) (LV2.V2 width height)
        pitch  = FCT.CInt $ fromIntegral (4 * width)
        iData  = CPI.imageData rgba8
        cmask = SVR.ABGR8888
    rawData <- liftIO $ stToIO $ DVS.thaw iData
    SVR.createRGBSurfaceFrom rawData dim pitch cmask

