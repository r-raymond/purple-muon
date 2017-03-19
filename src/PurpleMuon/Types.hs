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
Module      : PurpleMuon.Types
Description : Types used throughout the PurpleMuon library.
Copyright   : (c) Robin Raymond, 2016-2017
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX

This module defined types that are used in multiple subcomponents. Instead of
having different versions in each subcompoenent, they are collected here. As
such, this module is kind of a collection of things without a common
denominator.
-}

module PurpleMuon.Types
    ( FlType
    , Position(..)
    ) where

import           Protolude

import qualified Data.Binary as DBI
import qualified Linear.V2   as LV2

-- | The floating point type used in PurpleMuon
type FlType = Float

-- | A position type describing a position on the screen. Screen positions are
-- always int the range `[0,1] x [0,1]` and will be rescaled when drawn
-- according to the screen resolution. The physics engine uses the same
-- positions to describe the physical objects.
newtype Position = Position { unPosition :: LV2.V2 FlType }
    deriving (Generic, Eq, Show)

instance DBI.Binary Position
