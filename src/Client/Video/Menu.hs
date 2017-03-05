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

{-# LANGUAGE TemplateHaskell #-}

module Client.Video.Menu
    ( Position
    , MenuItem(..), position, mType
    , MenuType(..), labelText, labelSize, buttonText, buttonSize, inputFieldText, inputFieldSize
    ) where

import           Protolude

import qualified Client.Video.Types as CVT
import qualified Control.Lens       as CLE
import qualified Linear.V2          as LV2

type Position = LV2.V2 Float



data MenuItem
    = MenuItem
    { _position :: Position
    , _mType    :: MenuType
    }

data MenuType
    = Label
    { _labelText   :: Text
    , _labelSize   :: Int
    , _labeltextue :: CVT.TexUUID
    }
    | Button
    { _buttonText :: Text
    , _buttonSize :: LV2.V2 Float
    }
    | InputField
    { _inputFieldText :: Text
    , _inputFieldSize :: LV2.V2 Float
    }

CLE.makeLenses ''MenuItem
CLE.makeLenses ''MenuType
