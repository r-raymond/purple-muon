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
    , noFlip
    , renderGameObject
    , updateRenderInfo
    , relToAbs
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

-- | Draw sprite without flipping
noFlip :: SDL.V2 Bool
noFlip = SDL.V2 False False

relToAbs :: Resolution -> SDL.Rectangle PTY.FlType -> SDL.Rectangle FCT.CInt
relToAbs (SDL.V2 x y) (SDL.Rectangle (SDL.P (SDL.V2 px py)) (SDL.V2 sx sy)) =
    SDL.Rectangle (SDL.P $ SDL.V2 npx npy) (SDL.V2 nsx nsy)
      where 
        npx = FCT.CInt $ truncate (fromIntegral x * px)
        npy = FCT.CInt $ truncate (fromIntegral y * py)
        nsx = FCT.CInt $ truncate (fromIntegral x * sx)
        nsy = FCT.CInt $ truncate (fromIntegral y * sy)

-- | Render a sprite
--
-- TODO: The renderer as an argument is unnecessary if we take it out of the
-- Textureloader
renderSprite :: (MonadIO m, MonadError Text m)
             => SDL.Renderer
             -> CAS.SpriteLoaderType
             -> CAS.SpriteID
             -> Maybe (SDL.Rectangle FCT.CInt)
             -> FCT.CDouble
             -> SDL.V2 Bool
             -> m ()
renderSprite ren sl id mr phi flips = do
    CAS.Sprite t r c <- CAG.getAsset sl id
    tex <- CAG.getAsset (CAG.extData sl) t
    SDL.copyEx ren tex r mr phi (Just c) flips


renderGameObject :: (MonadIO m, MonadError Text m)
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
