module PurpleMuon.Game.Types
    ( GameObject(..)
    , GameObjUUID(..)
    ) where

{-|
Module      : PurpleMuon.Network.Types
Description : Collection of game object types
Copyright   : (c) Robin Raymond, 2016
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}

import Protolude

import qualified PurpleMuon.Physics.Types as PPT

newtype GameObjUUID = GameObjUUID { unGameObjUUID :: Word16 }
    deriving (Eq, Ord)


-- | A game object.
-- Has optionally a name and a `PhysicalObject`.
data GameObject
    = GameObject
    { _mName :: Maybe Text
    , _mPhOb :: Maybe PPT.PhyObjUUID
    }
