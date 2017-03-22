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
Module      : Client.Asset.Font
Description : Specialize Assetloading for Fonts
Copyright   : (c) Robin Raymond, 2016-2017
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}

module Client.Assets.Font
    ( FontLoaderType
    , FontID
    , FontSize(..)
    , fontLoader
    ) where


import           Protolude

import qualified Data.HashTable.IO          as DHI
import qualified SDL.Font                   as SFO
import qualified System.FilePath.Posix      as SFP

import qualified Client.Assets.Generic      as CAG
import qualified PurpleMuon.Util.MonadError as PUM

-- |A type of a font loader
type FontLoaderType = CAG.HashmapLoader SFO.Font FontSize

-- |Type of a font identifier
type FontID = CAG.AssetID (CAG.Asset FontLoaderType)

-- | The size of a font
newtype FontSize = FontSize { unFontSize :: Int }

-- | Implementation of `AssetLoader` for sounds.
fontLoader :: MonadIO m => FontSize -> m FontLoaderType
fontLoader size = do
    ht <- liftIO $ DHI.new
    return (CAG.HashmapLoader
        { CAG.store = ht
        , CAG.extraData = size
        , CAG.loadFromFile = \(FontSize s) p -> do
                        f <- try $ SFO.load p s
                                :: IO (Either SomeException SFO.Font)
                        let r = PUM.mapLeft show f
                            i = CAG.AssetID $ toS $ SFP.takeBaseName p
                        return (fmap (\x -> [(i, x)]) r)
        , CAG.delete = \_ s -> SFO.free s
        })
