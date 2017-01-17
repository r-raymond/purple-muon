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

module Client.Image
    ( parseTextureAtlas
    , filterAtlas
    ) where

import           Protolude

import qualified Text.XML.Light as TXL

parseTextureAtlas :: MonadIO m => FilePath -> m (Maybe TXL.Element)
parseTextureAtlas p = do
    c <- liftIO $ readFile p
    return (TXL.parseXMLDoc c)

filterAtlas :: Text -> TXL.Content -> Bool
filterAtlas t (TXL.Elem (TXL.Element (TXL.QName "SubTexture" Nothing Nothing) attr _ _)) =
    elem (TXL.Attr (TXL.QName "name" Nothing Nothing) (toS t)) attr
filterAtlas _ _ = False

data TextureAtlas

data Texture
    = Texture
    { atlas  :: TextureAtlas
    , name   :: Text
    , x      :: Int
    , y      :: Int
    , width  :: Int
    , height :: Int
    }
