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
    , getAsset
    , loadAsset
    , loadAsset_
    , loadAssets
    , loadAssets_
    , deleteAssets
    ) where

import           Protolude

import           Paths_purple_muon

import qualified Data.Binary                as DBI
import qualified Data.HashTable.IO          as DHI
import qualified System.FilePath.Posix      as SFP

import qualified PurpleMuon.Util.MonadError as PUM

-- | A unique identifier for assets.
-- The identifier has a phantom type which ensures that id's of different assets
-- can not be mixed.
newtype AssetID a = AssetID { unAssetID :: Text }
    deriving (Generic, Show)


data AssetLoader a b c
    = AssetLoader
    { store :: DHI.BasicHashTable Text a
    , extData :: b
    , load :: c -> b -> FilePath -> IO (Either Text [(AssetID a, a)])
    , delete :: a -> IO ()
    }


-- | Utility function to load a single asset
loadAsset :: (MonadIO m, MonadError Text m)
          => AssetLoader a b c
          -> c
          -> FilePath
          -> m [AssetID a]
loadAsset (AssetLoader s e l _) extra p = do
    np <- liftIO $ getDataFileName p
    massets <- liftIO $ l extra e np
    assets <- PUM.liftEither massets
    let insert ((AssetID id), asset) = DHI.insert s id asset
        insertAll = fmap insert assets
    liftIO $ sequence_ insertAll
    return (fmap (\((AssetID id), _) -> AssetID id) assets)


-- | Utility function to load a single asset for the special case of `c == ()`
loadAsset_ :: (MonadIO m, MonadError Text m)
           => AssetLoader a b ()
           -> FilePath
           -> m [AssetID a]
loadAsset_ al p = loadAsset al () p


-- | Utility function to retrieve a single asset
getAsset :: (MonadIO m, MonadError Text m)
         => AssetLoader a b c
         -> (AssetID a)
         -> m a
getAsset (AssetLoader s _ _ _) (AssetID t) = do
    ma <- liftIO $ DHI.lookup s t
    a <- PUM.liftMaybe ("Could not load asset " <> t) ma
    return a

-- | Utility function to delete an asset
deleteAsset :: (MonadIO m) => AssetLoader a b c -> AssetID a -> m ()
deleteAsset (AssetLoader s _ _ d) (AssetID t) = do
    ma <- liftIO $ DHI.lookup s t
    case ma of
        Just a -> liftIO $ do
            d a
            DHI.delete s t
        Nothing -> return ()

-- | Utility function for retrieving all assets
getAllIDs :: (MonadIO m) => AssetLoader a b c -> m [AssetID a]
getAllIDs (AssetLoader s _ _ _) = do
    l <- liftIO $ DHI.toList s
    return (fmap (AssetID . fst) l)


-- | Utility function to load a bunch of assets into an `AssetLoader` with a
-- callback function. Also normalizes the paths via the cabal paths.
loadAssets :: forall m a b c. (MonadError Text m, MonadIO m) 
              => AssetLoader a b c          -- ^ `AssetLoader` to load textures
                                            -- into
              -> c                          -- ^ extra load data
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
              -> m ()
loadAssets al ext paths callback = do
    -- pair with percentages
    let perc = fmap (\x -> 100 * x / (fromIntegral $ length paths)) [1 ..]
        pspe = zip paths perc
    -- functions for loading assets
        loadA :: (FilePath, Float) -> m ()
        loadA = \(p, per) -> do
            callback per (toS $ SFP.takeFileName p)
            void $ loadAsset al ext p
        loadAll = fmap loadA pspe
    sequence_ loadAll

-- | Same as `loadAssets` specialized for `c == ()`
loadAssets_ :: (MonadIO m, MonadError Text m)
            => AssetLoader a b ()
            -> [FilePath]
            -> (Float -> Text -> m ())
            -> m ()
loadAssets_ al p c = loadAssets al () p c

-- | Utility function to delete allassets in an `AssetLoader`
deleteAssets :: MonadIO m => AssetLoader a b c -> m ()
deleteAssets al = do
    ids <- getAllIDs al
    sequence_ (fmap (deleteAsset al) ids)

instance DBI.Binary (AssetID b)
