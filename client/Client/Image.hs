module Client.Image
    ( parseTextureAtlas
    , filterAtlas
    ) where

import Protolude

import qualified Text.XML.Light as TXL

parseTextureAtlas :: MonadIO m => FilePath -> m (Maybe TXL.Element)
parseTextureAtlas p = do
    c <- liftIO $ readFile p
    return (TXL.parseXMLDoc c)

filterAtlas :: Text -> TXL.Content -> Bool
filterAtlas t (TXL.Elem (TXL.Element (TXL.QName "SubTexture" Nothing Nothing) attr _ _)) =
    elem (TXL.Attr (TXL.QName "name" Nothing Nothing) (toS t)) attr
filterAtlas _ _ = False
