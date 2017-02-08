module PurpleMuon.Game.Types
    (
    ) where

{-|
Module      : PurpleMuon.Network.Types
Description : Collection of game object types
Copyright   : (c) Robin Raymond, 2016
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}

-- | A game object.
-- Has optionally a name, a `PhysicalObjects` and a `TexUUID`, i.e. a Texture
data GameObject
    { _uuid  :: Word16
    , _mName :: Maybe Text
    , _mPhOb :: Maybe PhysicalObject
    , _m
