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
    ( MenuItem(..), pos, mType
    , MenuType(..), labelSprite, button, buttonLabel, activeButton, activeLabel,
                    active, imageSprite
    , mkLabel
    , mkImage
    , mkButton
    , renderMenuItem
    ) where

import           Protolude

import qualified Control.Lens          as CLE
import qualified SDL
import qualified SDL.Font              as SFO

import qualified Client.Assets.Generic as CAG
import qualified Client.Assets.Sprite  as CAS
import qualified Client.Video.Sprite   as CVS
import qualified Client.Video.Types    as CVT
import qualified PurpleMuon.Types      as PTY

data MenuItem
    = MenuItem
    { _pos      :: SDL.Rectangle PTY.FlType
    , _mType    :: MenuType
    } deriving Show

data MenuType
    = Label
    { _labelSprite :: CAS.SpriteID
    }
    | Button
    { _button      :: CAS.SpriteID
    , _buttonLabel :: CAS.SpriteID
    , _activeButton :: CAS.SpriteID
    , _activeLabel  :: CAS.SpriteID
    , _active       :: Bool
    }
    | InputField
    {
    }
    | Image
    { _imageSprite :: CAS.SpriteID
    }
    deriving Show


mkFontTexture :: (MonadIO m, MonadError Text m)
              => CAS.SpriteLoaderType
              -> SDL.Renderer
              -> PTY.Color
              -> Text
              -> SFO.Font
              -> (CAG.AssetID CAS.Sprite)
              -> m ()
mkFontTexture sl r c t f id = do
    sprite <- SFO.blended f (PTY.unColor c) t
    tex <- SDL.createTextureFromSurface r sprite
    let tid = CAG.AssetID $ CAG.unAssetID id
    CAS.manualAdd sl (CAS.Sprite tid Nothing (SDL.P $ SDL.V2 0 0))
                  tex id

-- | Make a new button
mkButton :: (MonadIO m, MonadError Text m)
         => CAS.SpriteLoaderType            -- ^ used to store rendered text
         -> SDL.Renderer                    -- ^ used to upload texture
         -> PTY.Color                       -- ^ active text color
         -> PTY.Color                       -- ^ inactive text color
         -> CAS.SpriteID                    -- ^ active background
         -> CAS.SpriteID                    -- ^ inactive background
         -> PTY.Position                    -- ^ position
         -> PTY.Size                        -- ^ size
         -> Text                            -- ^ label
         -> SFO.Font                        -- ^ fonts to use
         -> m MenuItem
mkButton sl r ac ic ab ib p s t f = do
    let aId = CAG.AssetID $ "Button_ac:" <> t
        iId = CAG.AssetID $ "Button_in:" <> t
    mkFontTexture sl r ac t f aId
    mkFontTexture sl r ic t f iId

    return (MenuItem
        (SDL.Rectangle (SDL.P $ PTY.unPosition p) (PTY.unSize s))
        (Button ib iId ab aId False))

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
    let id = CAG.AssetID $ "Label:" <> t
    mkFontTexture sl r c t f id

    return (MenuItem
        (SDL.Rectangle (SDL.P $ PTY.unPosition p) (PTY.unSize s))
        (Label id))

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
               => CVT.Resolution
               -> CAS.SpriteLoaderType
               -> SDL.Renderer
               -> MenuItem
               -> m ()
renderMenuItem res sl r (MenuItem rect (Label s)) =
    CVS.renderSprite r sl s (Just sr) 0 CVS.noFlip
      where
        sr = CVS.relToAbs res rect

renderMenuItem res sl r (MenuItem rect (Image s)) =
    CVS.renderSprite r sl s (Just sr) 0 CVS.noFlip
      where
        sr = CVS.relToAbs res rect

renderMenuItem res sl r (MenuItem rect (Button ib iId ab aId a)) = do
    CVS.renderSprite r sl b (Just sr) 0 CVS.noFlip
    CVS.renderSprite r sl id (Just newsr) 0 CVS.noFlip
      where
        b = if a then ab else ib
        id = if a then aId else iId
        sr = CVS.relToAbs res rect
        SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h) = rect
        newp = SDL.P $ SDL.V2 (x + 0.125 * w) (y + 0.25 * h)
        newv = SDL.V2 (0.75 * w) (0.5 * h)
        newsr = CVS.relToAbs res (SDL.Rectangle newp newv)

CLE.makeLenses ''MenuType
CLE.makeLenses ''MenuItem
