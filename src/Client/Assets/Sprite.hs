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

module Client.Assets.Sprite
    ( spriteLoader
    , Sprite(..)
    , SpriteLoaderType
    , SpriteID
    ) where

import           Protolude

import qualified Data.HashTable.IO          as DHI
import qualified Foreign.C.Types            as FCT
import qualified SDL
import qualified Text.XML.Light             as TXL

import qualified Client.Assets.Generic      as CAG
import qualified Client.Assets.Texture      as CAT
import qualified PurpleMuon.Util.MonadError as PUM

data Sprite
    = Sprite
    { texture :: SDL.Texture
    , rect    :: SDL.Rectangle FCT.CInt
    , center  :: SDL.Point SDL.V2 FCT.CInt
    }

-- | Type of a sprite loader
type SpriteLoaderType = CAG.AssetLoader Sprite CAT.TextureLoaderType ()

-- | Type of a sprite loader identifier
type SpriteID = CAG.AssetID Sprite

-- | Implementation of `AssetLoader` for sprites
spriteLoader :: MonadIO m => CAT.TextureLoaderType -> m SpriteLoaderType
spriteLoader tlo = do
    ht <- liftIO DHI.new
    return CAG.AssetLoader
        { CAG.store = ht
        , CAG.extData = tlo
        , CAG.load = \_ lo p -> do
                            esp <- runExceptT $ loadSpriteAtlas lo p
                            let y = fmap (fmap (\(CAG.AssetID i, s) -> (CAG.AssetID i, s))) esp
                            return y
        , CAG.delete = \_ -> return ()
                -- TODO: Implement. After deleting the sprite, check if there
                -- might be no use of the texture left and if so, also delete it
        }

-- | Helper function to load a sprite atlas
loadSpriteAtlas :: (MonadError Text m, MonadIO m)
                => CAT.TextureLoaderType
                -> FilePath
                -> m [(SpriteID, Sprite)]
loadSpriteAtlas tlo p = do
    xml <- loadXMLDocument p
    let filtered = filter contentFilter (TXL.elContent xml)
    tid <- parseSpriteAtlasHeader tlo xml
    tex <- CAG.getAsset tlo tid
    sequence (fmap (parseSprite tex) filtered)

-- | Load an XML Document from a file with error handling
loadXMLDocument :: (MonadError Text m, MonadIO m)
                  => FilePath
                  -> m TXL.Element
loadXMLDocument p = do
    c <- liftIO $ readFile p
    PUM.liftMaybe ("Error parsing " <> toS p) (TXL.parseXMLDoc c)

-- | Helper to filter out unwanted contents
contentFilter :: TXL.Content -> Bool
contentFilter (TXL.Elem _) = True
contentFilter _            = False

-- |Helper function to parse the surrounding xml element. This function will
-- also load the requested texture into the TextureLoader.
parseSpriteAtlasHeader :: (MonadError Text m, MonadIO m)
                        => CAT.TextureLoaderType
                        -> TXL.Element
                        -> m CAT.TextureID
parseSpriteAtlasHeader tlo x = do
    let atts = TXL.elAttribs x
    path <- PUM.liftMaybe "Error parsing Texture atlas header"
            (xmlAttrHelper atts "imagePath")
    ids <- CAG.loadAsset_ tlo (toS path)
    PUM.liftList "Error parsing header" ids

-- |Helper function that filters a attribute list for a given key.
xmlAttrHelper :: [TXL.Attr] -> Text -> Maybe Text
xmlAttrHelper atts key =
    fmap (toS . TXL.attrVal)
         (find (\x -> TXL.attrKey x == TXL.QName (toS key) Nothing Nothing) atts)


-- |Parse a single sprite xml element
parseSprite :: (MonadError Text m)
            => SDL.Texture
            -> TXL.Content
            -> m (SpriteID, Sprite)
parseSprite tex (TXL.Elem (TXL.Element (TXL.QName "SubTexture" Nothing Nothing) attr _ _)) = do
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
    return (CAG.AssetID name,
            Sprite
                tex
                (SDL.Rectangle (SDL.P $ SDL.V2 xInt yInt) (SDL.V2 wInt hInt))
                (SDL.P $ SDL.V2 (wInt `div` 2) (hInt `div` 2)))
parseSprite _ t = throwError ("Error parsing texture" <> show t)

