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
    , mkImage
    , renderMenuItem
    ) where

import           Protolude

import qualified SDL
import qualified SDL.Font              as SFO

import qualified Client.Assets.Generic as CAG
import qualified Client.Assets.Sprite  as CAS
import qualified Client.Video.Sprite   as CVS
import qualified Client.Video.Types    as CVT
import qualified PurpleMuon.Types      as PTY

data MenuItem
    = MenuItem
    { pos      :: SDL.Rectangle PTY.FlType
    , mType    :: MenuType
    } deriving Show

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
    | Image
    { imageSprite :: CAS.SpriteID
    }
    deriving Show

-- | Make a new label
mkLabel :: (MonadIO m, MonadError Text m)
        => CAS.SpriteLoaderType             -- ^ used to store rendered text
        -> SDL.Renderer                     -- ^ renderer used to upload texture
        -> PTY.Color                        -- ^ color of text (RGBA)
        -> PTY.Position                     -- ^ position of label
        -> PTY.Size                         -- ^ size of the label
        -> Text                             -- ^ text of label
        -> SFO.Font                         -- ^ used font
        -> m MenuItem
mkLabel sl r c p s t f = do
    sur <- SFO.blended f (PTY.unColor c) t
    tex <- SDL.createTextureFromSurface r sur
    let id = "Label:" <> t
    CAS.manualAdd sl (CAS.Sprite (CAG.AssetID id) Nothing (SDL.P $ SDL.V2 0 0))
                  tex (CAG.AssetID id)
    return (MenuItem
        (SDL.Rectangle (SDL.P $ PTY.unPosition p) (PTY.unSize s))
        (Label (CAG.AssetID id)))

-- | Make a new image
mkImage :: CAS.SpriteID
        -> PTY.Position
        -> PTY.Size
        -> MenuItem
mkImage id p s = MenuItem
        (SDL.Rectangle (SDL.P $ PTY.unPosition p) (PTY.unSize s))
        (Image id)

-- | Render a menu item
renderMenuItem :: (MonadIO m, MonadError Text m)
               => CAS.SpriteLoaderType
               -> SDL.Renderer
               -> MenuItem
               -> m ()
renderMenuItem sl r (MenuItem rect (Label s)) =
    CVS.renderSprite r sl s (Just sr) 0 CVS.noFlip
      where
        sr = CVS.relToAbs (CVT.Resolution $ SDL.V2 800 600) rect -- TODO: Replace with resolution

renderMenuItem sl r (MenuItem rect (Image s)) =
    CVS.renderSprite r sl s (Just sr) 0 CVS.noFlip
      where
        sr = CVS.relToAbs (CVT.Resolution $ SDL.V2 800 600) rect -- TODO: Replace with resolution
