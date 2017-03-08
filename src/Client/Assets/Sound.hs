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

module Client.Assets.Sound
    ( soundLoader
    , soundAssets
    ) where

import           Protolude

import qualified Data.HashTable.IO          as DHI
import qualified SDL.Mixer                  as SMI
import qualified System.FilePath.Posix      as SFP

import qualified Client.Assets.Generic      as CAG
import qualified PurpleMuon.Util.MonadError as PUM

type SoundLoaderType = CAG.HashmapLoader SMI.Chunk ()

-- | Implementation of `AssetLoader` for sounds.
soundLoader :: MonadIO m => m SoundLoaderType
soundLoader = do
    ht <- liftIO $ DHI.new
    return (CAG.HashmapLoader
        { CAG.store = ht
        , CAG.extraData = ()
        , CAG.loadFromFile = \_ p -> do
                        s <- try $ SMI.load p
                                :: IO (Either SomeException SMI.Chunk)
                        let r = PUM.mapLeft show s
                            i = CAG.AssetID $ toS $ SFP.takeBaseName p
                        return (fmap (\x -> [(i, x)]) r)
        , CAG.delete = \_ s -> SMI.free s
        })


-- | Complete list of all sound assets in the game
soundAssets :: [FilePath]
soundAssets =
    [ "res/ogg/click1.ogg"
    , "res/ogg/click2.ogg"
    , "res/ogg/rollover1.ogg"
    , "res/ogg/rollover2.ogg"
    , "res/ogg/switch2.ogg"
    , "res/ogg/switch3.ogg"
    ]
