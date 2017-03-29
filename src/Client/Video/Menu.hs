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
    ( MenuItem(..)
    , MenuType(..)
    , mkLabel
    ) where

import           Protolude

import qualified SDL
import qualified SDL.Font              as SFO

import qualified Client.Assets.Generic as CAG
import qualified Client.Assets.Sprite  as CAS
import qualified PurpleMuon.Types      as PTY

data MenuItem
    = MenuItem
    { position :: PTY.Position
    , mType    :: MenuType
    }

data MenuType
    = Label
    { labelSprite :: CAS.SpriteID
    }
    | Button
    { button      :: CAS.SpriteID
    , buttonLabel :: CAS.SpriteID
    }
    | InputField
    {
    }

-- | Make a new label
mkLabel :: (MonadIO m, MonadError Text m)
        => CAS.SpriteLoaderType             -- ^ used to store rendered text
        -> SDL.Renderer                     -- ^ renderer used to upload texture
        -> PTY.Color                        -- ^ color of text (RGBA)
        -> PTY.Position                     -- ^ position of label
        -> Text                             -- ^ text of label
        -> SFO.Font                         -- ^ used font
        -> m MenuItem
mkLabel sl r c p t f = do
    sur <- SFO.blended f (PTY.unColor c) t
    tex <- SDL.createTextureFromSurface r sur
    let id = "Label:" <> t
    CAS.manualAdd sl (CAS.Sprite (CAG.AssetID id) Nothing (SDL.P $ SDL.V2 0 0))
                  tex (CAG.AssetID id)
    return (MenuItem p (Label (CAG.AssetID id)))

