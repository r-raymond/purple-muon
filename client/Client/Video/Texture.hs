{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Client.Video.Texture
    ( parseTextureAtlas
    , newTextureLoader
    , loadTextureAtlas
    , renderTexture
    , loadSurface
    , surfaceToTexture
    , addTextureAtlas
    , getTexture
    ) where

import           Protolude

import qualified Codec.Picture        as CPI
import qualified Data.IntMap.Strict   as DIS
import qualified Data.Vector.Storable as DVS
import qualified Foreign.C.Types      as FCT
import qualified Linear.Affine        as LAF
import qualified Linear.V2            as LV2
import qualified SDL                  as SDL
import qualified SDL.Vect             as SVE
import qualified SDL.Video.Renderer   as SVR
import qualified Text.XML.Light       as TXL

import qualified Client.Util          as CUT
import qualified Client.Video.Types   as CVT

renderTexture :: (MonadIO m) => CVT.TextureLoader -> CVT.TexUUID -> Maybe (SVR.Rectangle FCT.CInt) -> m ()
renderTexture (CVT.TextureLoader at te r _) (CVT.TexUUID u) mr =
    case DIS.lookup u te of
        Nothing -> do   -- render red square for missing
            SVR.rendererDrawColor r SDL.$= SVE.V4 255 0 0 0
            SVR.fillRect r mr
        Just t  -> SVR.copy r (CVT.unTextureAtlas (at DIS.! CVT.atlas t))
                              (Just $ CVT.rect t) mr


getTexture :: CVT.TextureLoader -> Text -> Maybe CVT.TexUUID
getTexture (CVT.TextureLoader _ t _ _)  p =
    fmap (CVT.TexUUID . fst) (find (\(_, x) -> CVT.name x == p) (DIS.assocs t))

newTextureLoader :: SVR.Renderer -> CVT.TextureLoader
newTextureLoader r = CVT.TextureLoader DIS.empty DIS.empty r 0

addTextureAtlas :: (MonadError Text m, MonadIO m)
                => CVT.TextureLoader
                -> FilePath
                -> m CVT.TextureLoader
addTextureAtlas (CVT.TextureLoader at te r k) p = do
    (newAt, newTex) <- parseTextureAtlas r k p
    let newAtMap = DIS.insert k newAt at
        -- Assume a maximum of 1000 Textures per Atlas (TODO: Make this less
        -- dumb)
        newTeKeys = fmap ((1000*k) +) [1..]
        h1        = fmap DIS.insert newTeKeys
        h2        = zip h1 newTex
        h3        = fmap (\(x,y) -> x y) h2
        newTeMap  = foldr (\x y -> x y) te h3
    return $ CVT.TextureLoader newAtMap newTeMap r (k+1)


parseTextureAtlas :: (MonadError Text m, MonadIO m)
                  => SVR.Renderer
                  -> CVT.AtlasUUID
                  -> FilePath
                  -> m (CVT.TextureAtlas, [CVT.Texture])
parseTextureAtlas r k p = do
    xml <- loadTextureAtlas p
    let texs = TXL.elContent xml
    header <- parseTextureAtlasHeader r xml
    atts <- sequence (fmap (parseTexture k) texs)
    return (header, atts)


parseTextureAtlasHeader :: (MonadError Text m, MonadIO m)
                        => SVR.Renderer
                        -> TXL.Element
                        -> m CVT.TextureAtlas
parseTextureAtlasHeader r x = do
    let atts = TXL.elAttribs x
    path <- CUT.liftMaybe "Error parsing Texture atlas header" (xmlAttrHelper atts "imagePath")
    surf <- loadSurface (toS path)
    tex  <- surfaceToTexture r surf
    return $ CVT.TextureAtlas tex


xmlAttrHelper :: [TXL.Attr] -> Text -> Maybe Text
xmlAttrHelper atts key =
    fmap (toS . TXL.attrVal)
         (find (\x -> TXL.attrKey x == TXL.QName (toS key) Nothing Nothing) atts)

parseTexture :: (MonadError Text m) => CVT.AtlasUUID -> TXL.Content -> m CVT.Texture
parseTexture k (TXL.Elem (TXL.Element (TXL.QName "SubTexture" Nothing Nothing) attr _ _)) = do
    let liftHelper x = CUT.liftMaybe ("Error parsing attribute " <> x) $ xmlAttrHelper attr x
        parseHelper x = CUT.liftMaybe ("Error parsing attribute " <> x) $ readMaybe $ toS x 
    name   <- liftHelper "name"
    x      <- liftHelper "x"
    y      <- liftHelper "y"
    width  <- liftHelper "width"
    height <- liftHelper "height"
    xInt   <- parseHelper x
    yInt   <- parseHelper y
    wInt   <- parseHelper width
    hInt   <- parseHelper height
    return $ CVT.Texture k name (SVR.Rectangle (LAF.P $ LV2.V2 xInt yInt) (LV2.V2 wInt hInt))
parseTexture _ _ = throwError "Error parsing texture"

loadTextureAtlas :: (MonadError Text m, MonadIO m)
                  => FilePath
                  -> m TXL.Element
loadTextureAtlas p = do
    c <- liftIO $ readFile p
    CUT.liftMaybe ("Error parsing " <> toS p) (TXL.parseXMLDoc c)

surfaceToTexture :: MonadIO m => SVR.Renderer -> SVR.Surface -> m SVR.Texture
surfaceToTexture = SVR.createTextureFromSurface

loadSurface :: (MonadError Text m, MonadIO m) => FilePath -> m SVR.Surface
loadSurface p = do
    mImg <- liftIO $ CPI.readImage p
    img <- CUT.liftEitherWith (\err -> "Could not load image: " <> toS err) mImg
    loadSurfaceHelper img

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

