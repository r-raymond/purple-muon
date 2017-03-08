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

module Client.Assets.Generic
    ( AssetID(..)
    , AssetLoader(..)
    , HashmapLoader(..)
    ) where

import Protolude

import           Paths_purple_muon

import qualified System.FilePath.Posix as SFP
import qualified Data.HashTable.IO as DHI

import qualified PurpleMuon.Util.MonadError as PUM

-- | A unique identifier for assets.
-- The identifier has a phantom type which ensures that id's of different assets
-- can not be mixed.
newtype AssetID a = AssetID { unAssetID :: Text }

-- | A AssetLoader is responsible for managing assets. It can load assets and
-- store them for easy retrival.
class AssetLoader al where
    -- | Type of an asset
    data Asset al :: *
    -- | Load an asset (maybe multiple assets) into an AssetLoader
    loadAsset :: (MonadIO m, MonadError Text m)  => al -> FilePath ->  m al
    -- | Retreive asset from AssetLoader
    getAsset :: (MonadIO m, MonadError () m) => al -> AssetID (Asset al) -> m (Asset al)
    -- | Delete an asset
    deleteAsset :: (MonadIO m) => al -> AssetID (Asset al) -> m ()

-- | A simple implementation of an AssetLoader via Hashmaps
-- `a` is the type of the Asset to be loaded and stored and `ext` can be any
-- extra data that needs to be used when loading an asset from a file.
data HashmapLoader a ext
    = HashmapLoader
    { store :: DHI.BasicHashTable Text a
    , extraData :: ext
    , loadFromFile :: ext -> FilePath -> IO (Either Text [(AssetID a, a)])
    , delete :: ext -> a -> IO ()
    }

instance AssetLoader (HashmapLoader a ext) where

    data Asset (HashmapLoader a ext) = Asset a

    loadAsset h@(HashmapLoader s e l _) p = do
        massets <- liftIO $ l e p
        assets <- PUM.liftEither massets
        let insert ((AssetID id), asset) = DHI.insert s id asset
            insertAll = fmap insert assets
        liftIO $ sequence_ insertAll
        return (h { store = s })

    getAsset (HashmapLoader s _ _ _) (AssetID t) = do
        ma <- liftIO $ DHI.lookup s t
        a <- PUM.liftMaybe () ma
        return (Asset a)

    deleteAsset (HashmapLoader s e _ d) (AssetID t) = do
        ma <- liftIO $ DHI.lookup s t
        case ma of
            Just a -> liftIO $ do
                d e a
                DHI.delete s t
            Nothing -> return ()


-- | Utility function to load a bunch of assets into an `AssetLoader` with a
-- callback function.
loadAssets :: forall m a. (MonadError Text m, MonadIO m, AssetLoader a)
              => a                          -- ^ `AssetLoader` to load textures
                                            -- into
              -> [FilePath]                 -- ^ Paths to asset files. Note that
                                            -- these should be specified
                                            -- relative to the root of the git
                                            -- directory
              -> (Float -> Text -> m ())    -- ^ Callback function. This
                                            -- function will be called whenever
                                            -- a new asset is loading with the
                                            -- percentage of loaded files and
                                            -- the name of the currently loading
                                            -- asset. If this is not needed,
                                            -- set it to `return ()`.
              -> m a
loadAssets al paths callback = do
    -- convert names to file paths
    ps <- liftIO $ sequence (fmap getDataFileName paths)
    -- pair with percentages
    let perc = fmap (\x -> 100 * x / (fromIntegral $ length ps)) [1 ..]
        pspe = zip ps perc
    -- functions for loading assets
        loadA :: (FilePath, Float) -> a -> m a
        loadA = \(p, per) tlo -> do
            callback per (toS $ SFP.takeFileName p)
            loadAsset al p
        loadAll = fmap loadA pspe
    foldl' (>>=) (return al) loadAll
