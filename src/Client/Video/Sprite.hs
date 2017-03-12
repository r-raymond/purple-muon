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

module Client.Video.Sprite
    (
    ) where

import           Protolude

import qualified Foreign.C.Types       as FCT
import qualified SDL

import qualified Client.Assets.Generic as CAG
import qualified Client.Assets.Sprite  as CAS
import qualified PurpleMuon.Game.Types as PGT

type Resolution = (Int, Int)

-- | Render a sprite
--
-- TODO: The renderer as an argument is unnecessary if we take it out of the
-- Textureloader
renderSprite :: (MonadIO m)
             => SDL.Renderer
             -> CAS.SpriteLoaderType
             -> CAS.SpriteID
             -> Maybe (SDL.Rectangle FCT.CInt)
             -> FCT.CDouble
             -> SDL.V2 Bool
             -> m ()
renderSprite ren sl id mr phi flips = do
    es <- runExceptT $ CAG.getAsset sl id
    case es of
        (Right (CAG.A (CAS.Sprite t r c))) -> do
                SDL.copyEx ren t (Just r) mr phi (Just c) flips
        (Left t) -> panic t


renderGameObject :: MonadIO m
                 => SDL.Renderer
                 -> CAS.SpriteLoaderType
                 -> Resolution
                 -> PGT.GameObject
                 -> m ()
renderGameObject ren sl res (PGT.GameObject _ _ _ sp) =
    case sp of
        Just (s, pos, size) -> renderSprite ren sl s (Just apos) a (SDL.V2 False False)
          where
            x = PGT._xPos pos
            y = PGT._yPos pos
            a = PGT._angle pos
            xS = PGT._xSize size
            yS = PGT._ySize size
        Nothing -> return ()
