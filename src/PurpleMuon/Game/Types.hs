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

{-# LANGUAGE TemplateHaskell #-}

module PurpleMuon.Game.Types
    ( GameObject(..), goData, mName, mPhOb, mReInfo
    , GameObjectData(..)
    , GameObjUUID(..)
    , Size(..), xSize, ySize
    , RenderInfo(..), pos, angle, size, sprite
    ) where

import           Protolude

import qualified Control.Lens             as CLE
import qualified Data.Binary              as DBI
import qualified Linear.V2                as LV2

import qualified Client.Assets.Sprite     as CAS
import qualified PurpleMuon.Physics.Types as PPT
import qualified PurpleMuon.Types         as PTY

newtype GameObjUUID = GameObjUUID { unGameObjUUID :: Word16 }
    deriving (Eq, Ord)

-- | The type of the game object and the data that goes with it.
data GameObjectData
    = PlayerShip
    | Comet
    deriving (Generic)

data RenderInfo
    = RenderInfo
    { _pos    :: PTY.Position
    , _angle  :: PTY.FlType
    , _size   :: LV2.V2 PTY.FlType
    , _sprite :: CAS.SpriteID
    } deriving Generic

data Size
    = Size
    { _xSize :: Float
    , _ySize :: Float
    } deriving Generic


-- | A game object.
-- Has optionally a name and a `PhysicalObject`.
data GameObject
    = GameObject
    { _goData  :: GameObjectData
    , _mName   :: Maybe Text
    , _mPhOb   :: Maybe PPT.PhyObjUUID
    , _mReInfo :: Maybe RenderInfo
    } deriving (Generic)

instance DBI.Binary GameObjectData
instance DBI.Binary Size
instance DBI.Binary RenderInfo
instance DBI.Binary GameObject

CLE.makeLenses ''GameObject
CLE.makeLenses ''RenderInfo
CLE.makeLenses ''Size
