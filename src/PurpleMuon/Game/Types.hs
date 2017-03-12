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
Module      : PurpleMuon.Network.Types
Description : Collection of game object types
Copyright   : (c) Robin Raymond, 2016-2017
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX
-}

module PurpleMuon.Game.Types
    ( GameObject(..)
    , GameObjectData(..)
    , GameObjUUID(..)
    ) where

import           Protolude

import qualified Data.Binary              as DBI

import qualified Client.Assets.Sprite     as CAS
import qualified PurpleMuon.Physics.Types as PPT

newtype GameObjUUID = GameObjUUID { unGameObjUUID :: Word16 }
    deriving (Eq, Ord)

-- | The type of the game object and the data that goes with it.
data GameObjectData
    = PlayerShip
    | Comet
    deriving (Generic)

-- | A game object.
-- Has optionally a name and a `PhysicalObject`.
data GameObject
    = GameObject
    { _goData  :: GameObjectData
    , _mName   :: Maybe Text
    , _mPhOb   :: Maybe PPT.PhyObjUUID
    , _mSprite :: Maybe CAS.SpriteID
    } deriving (Generic)

instance DBI.Binary GameObjectData
instance DBI.Binary GameObject
