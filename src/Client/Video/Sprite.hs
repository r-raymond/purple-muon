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
    ( renderSprite
    , renderGameObject
    , updateRenderInfo
    ) where

import           Protolude

import qualified Control.Lens             as CLE
import qualified Data.IntMap.Strict       as DIS
import qualified Foreign.C.Types          as FCT
import qualified SDL

import qualified Client.Assets.Generic    as CAG
import qualified Client.Assets.Sprite     as CAS
import qualified PurpleMuon.Game.Types    as PGT
import qualified PurpleMuon.Physics.Types as PPT
import qualified PurpleMuon.Types         as PTY

type Resolution = SDL.V2 Int

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
renderGameObject ren sl res (PGT.GameObject _ _ _ mri) =
    case mri of
        Just (PGT.RenderInfo p a si sp) ->
            renderSprite ren sl sp (Just apos) an (SDL.V2 False False)
              where
                xy = (fmap fromIntegral res) * (PTY.unPosition p)
                an = FCT.CDouble $ float2Double a
                xyS = (fmap fromIntegral res) * si
                v1 = fmap truncate xy
                v2 = fmap truncate xyS
                apos = SDL.Rectangle (SDL.P v1) v2
        Nothing -> return ()

-- | Update the sprite position with the position of the physical Object
updateRenderInfo :: PPT.PhysicalObjects -> PGT.GameObject -> PGT.GameObject
updateRenderInfo pos go@(PGT.GameObject _ _ (Just po) (Just ri)) =
    case newp of
        (Just np) -> go { PGT._mReInfo = Just $ CLE.set PGT.pos npos ri }
          where
            npos = CLE.view PPT.pos np
        Nothing -> go
      where
        newp = DIS.lookup (PTY.unKey po) pos
updateRenderInfo _ go = go
