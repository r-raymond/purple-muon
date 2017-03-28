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
    , toFontID
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
type FontLoaderType = CAG.AssetLoader SFO.Font () FontSize

-- |Type of a font identifier
type FontID = CAG.AssetID SFO.Font

-- | The size of a font
newtype FontSize = FontSize { unFontSize :: Int }

-- | Map a fontname + size to a font id
toFontID :: FilePath -> FontSize -> FontID
toFontID p (FontSize s) = CAG.AssetID $
    ( toS $ SFP.takeBaseName p
   <> ":"
   <> show s
    )

-- | Implementation of `AssetLoader` for sounds.
fontLoader :: MonadIO m => m FontLoaderType
fontLoader = do
    ht <- liftIO $ DHI.new
    return (CAG.AssetLoader
        { CAG.store = ht
        , CAG.extData = ()
        , CAG.load = \s _ p -> do
                        f <- try $ SFO.load p (unFontSize s)
                                :: IO (Either SomeException SFO.Font)
                        let r = PUM.mapLeft show f
                            i = toFontID p s
                        return (fmap (\x -> [(i, x)]) r)
        , CAG.delete = SFO.free
        })
