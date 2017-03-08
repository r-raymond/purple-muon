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
Module      : Client.Video.Texture
Description : Texture handling utitlities.
Copyright   : (c) Robin Raymond, 2016-2017
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX

This module uses SDL2 and JuicyPixels to work with textures. JuicyPixels is used
to load arbitrary image formats. These are then converted to RGBA8 images which
SDL2 uses to upload to the video card.

All image files need to have an accompanying texture atlas file. This file needs
to satisfy the following criteria:

    * it needs to be a valid xml file
    * it has to consist of a single outer xml element which has the property
    `imagePath`, with value the path of the actual image file (specified from
    the root of the git repository)
    * the content of the single outer xml element has to be more xml elements,
    which have the following properties:

        * `name`: The name that is used to load this file
        * `x`: The `x` offset in the image specified by `imagePath`
        * `y`: The `y` offset in the image specified by `imagePath`
        * `width`: The width of the texture
        * `height`: The height of the texture

The names of the xml elements are arbitrary. Anything besides the above
mentioned will be ignored and can safely be used to encode additional
information.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Client.Video.Texture
    ( newTextureLoader
    , renderTexture
    , addTextureAtlas
    , getTexture
    , deleteAllTextures
    ) where

import           Protolude

import           Paths_purple_muon

import qualified Control.Exception          as CEX
import qualified Data.IntMap.Strict         as DIS
import qualified Data.Vector.Storable       as DVS
import qualified Foreign.C.Types            as FCT
import qualified Linear.Affine              as LAF
import qualified Linear.V2                  as LV2
import qualified SDL                        as SDL
import qualified SDL.Image                  as SIM
import qualified SDL.Vect                   as SVE
import qualified SDL.Video.Renderer         as SVR
import qualified Text.XML.Light             as TXL

import qualified Client.Video.Types         as CVT
import qualified PurpleMuon.Util.MonadError as PUM

-- | Render a texture specified by a `CVT.TexUUID`.
renderTexture :: (MonadIO m) => CVT.TextureLoader -> CVT.TexUUID -> Maybe (SVR.Rectangle FCT.CInt) -> m ()
renderTexture (CVT.TextureLoader at te r _ _) (CVT.TexUUID u) mr =
    case DIS.lookup u te of
        Nothing -> do   -- render red square for missing
            SVR.rendererDrawColor r SDL.$= SVE.V4 255 0 0 0
            SVR.fillRect r mr
        Just t  -> SVR.copy r (CVT.unTextureAtlas (at DIS.! CVT.atlas t))
                              (Just $ CVT.rect t) mr

-- | Delete all textures saved in the given texture atlas
deleteAllTextures :: (MonadIO m) => CVT.TextureLoader -> m ()
deleteAllTextures = undefined

-- | Upload one texture.
-- Can specify an optional TexUUID. In case it is provided, that one is deleted
-- and replaced with the new Texture. There is no guarantee that the returned
-- TexUUID will be the same though.
uploadTexture :: (MonadIO m)
              => CVT.TextureLoader
              -> SVR.Surface
              -> Maybe CVT.TexUUID
              -> m CVT.TexUUID
uploadTexture tl s Nothing = undefined

-- |Find the `CVT.TexUUID` of a texture.
getTexture :: CVT.TextureLoader -> Text -> Maybe CVT.TexUUID
getTexture (CVT.TextureLoader _ t _ _ _)  p =
    fmap (CVT.TexUUID . fst) (find (\(_, x) -> CVT.name x == p) (DIS.assocs t))

-- |Create a new texture loader.
newTextureLoader :: SVR.Renderer -> CVT.TextureLoader
newTextureLoader r = CVT.TextureLoader DIS.empty DIS.empty r 0 (CVT.TexUUID 0)

-- |Add an texture atlas to a texture loader
addTextureAtlas :: (MonadError Text m, MonadIO m)
                => CVT.TextureLoader
                -> FilePath
                -> m CVT.TextureLoader
addTextureAtlas (CVT.TextureLoader at te r k n) p = do
    (newAt, newTex) <- parseTextureAtlas r k p
    let newAtMap = DIS.insert k newAt at
        newTeKeys = fmap ((1000*k + (CVT.unTexUUID n)) +) [1..]
        h1        = fmap DIS.insert newTeKeys
        h2        = zip h1 newTex
        h3        = fmap (\(x,y) -> x y) h2
        newTeMap  = foldr (\x y -> x y) te h3
        newN      = CVT.TexUUID (length newTex + (CVT.unTexUUID n))
    return $ CVT.TextureLoader newAtMap newTeMap r (k+1) newN

-- |Helper function for addTextureAtlas
parseTextureAtlas :: (MonadError Text m, MonadIO m)
                  => SVR.Renderer
                  -> CVT.AtlasUUID
                  -> FilePath
                  -> m (CVT.TextureAtlas, [CVT.Texture])
parseTextureAtlas r k p = do
    xml <- loadTextureAtlas p
    let texs = filter contentFilter (TXL.elContent xml)
    header <- parseTextureAtlasHeader r xml
    atts <- sequence (fmap (parseTexture k) texs)
    return (header, atts)

contentFilter :: TXL.Content -> Bool
contentFilter (TXL.Elem _) = True
contentFilter _            = False

-- |Helper function for parseTextureAtlas
parseTextureAtlasHeader :: (MonadError Text m, MonadIO m)
                        => SVR.Renderer
                        -> TXL.Element
                        -> m CVT.TextureAtlas
parseTextureAtlasHeader r x = do
    let atts = TXL.elAttribs x
    path <- PUM.liftMaybe "Error parsing Texture atlas header" (xmlAttrHelper atts "imagePath")
    surf <- loadSurface (toS path)
    tex  <- surfaceToTexture r surf
    return $ CVT.TextureAtlas tex


xmlAttrHelper :: [TXL.Attr] -> Text -> Maybe Text
xmlAttrHelper atts key =
    fmap (toS . TXL.attrVal)
         (find (\x -> TXL.attrKey x == TXL.QName (toS key) Nothing Nothing) atts)

parseTexture :: (MonadError Text m) => CVT.AtlasUUID -> TXL.Content -> m CVT.Texture
parseTexture k (TXL.Elem (TXL.Element (TXL.QName "SubTexture" Nothing Nothing) attr _ _)) = do
    let liftHelper x = PUM.liftMaybe ("Error parsing attribute " <> x) $ xmlAttrHelper attr x
        parseHelper x = PUM.liftMaybe ("Error parsing attribute " <> x) $ readMaybe $ toS x
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
parseTexture _ t = throwError ("Error parsing texture" <> show t)

loadTextureAtlas :: (MonadError Text m, MonadIO m)
                  => FilePath
                  -> m TXL.Element
loadTextureAtlas p = do
    c <- liftIO $ readFile p
    PUM.liftMaybe ("Error parsing " <> toS p) (TXL.parseXMLDoc c)

-- | Turn a surface into a texture. Also delete the surface
surfaceToTexture :: MonadIO m => SVR.Renderer -> SVR.Surface -> m SVR.Texture
surfaceToTexture r s = do
    tex <- SVR.createTextureFromSurface r s
    SDL.freeSurface s
    return tex

loadSurface :: (MonadError Text m, MonadIO m) => FilePath -> m SVR.Surface
loadSurface p = do
    img <- liftIO (CEX.try $ SIM.load p :: IO (Either SomeException SDL.Surface))
    PUM.liftEitherWith (const ("Could not load file " <> (toS p))) img

