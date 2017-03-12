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
                 => SDL.Render
                 -> Resolution
                 -> PGT.GameObject
                 -> m ()

renderGameObjects :: CVTY.TexUUID -> PPT.PhysicalObject -> CTY.Game ()
renderGameObjects t po = do
    res <- ask
    sta <- get
    let pos = PPT.unPosition $ CLE.view PPT.pos po
        window = CLE.view CTY.window res
        texload = CLE.view CTY.textures sta
    windowsize <- SDL.get $ SDL.windowSize window

    -- TODO: Fix this with actual physical size
    let coord = fmap truncate (pos * fmap fromIntegral windowsize)
        size = fmap FCT.CInt (SDL.V2 10 10)
        p    = fmap FCT.CInt coord
        bb   = Just $ SDL.Rectangle (SDL.P p) size

    CVT.renderTexture texload t bb

physicalObjectOfGameObject :: PGT.GameObject -> CTY.Game (Maybe PPT.PhysicalObject)
physicalObjectOfGameObject (PGT.GameObject _ _ p) = do
    sta <- get
    let pos = CLE.view (CTY.game . CTY.physicalObjects) sta
    return (p >>= \x -> DIS.lookup (fromIntegral x) pos)

renderGameObject :: PGT.GameObject -> CTY.Game ()
renderGameObject go@(PGT.GameObject PGT.Comet _ _) = do
    res <- ask
    sta <- get
    p <- physicalObjectOfGameObject go

    case p of
        (Just po) -> do
            let pos = PPT.unPosition $ CLE.view PPT.pos po
                window = CLE.view CTY.window res
                texload = CLE.view CTY.textures sta
            windowsize <- SDL.get $ SDL.windowSize window

            -- TODO: Fix this with actual physical size
            let coord = fmap truncate (pos * fmap fromIntegral windowsize)
                size = fmap FCT.CInt (SDL.V2 10 10)
                poi  = fmap FCT.CInt coord
                bb   = Just $ SDL.Rectangle (SDL.P poi) size
                (Just t) = CVT.getTexture texload "meteorBrown_big1.png"

            CVT.renderTexture texload t bb
        Nothing -> -- wtf, can't render a comet without a physical position
            -- todo: log error
            return ()
renderGameObject (PGT.GameObject PGT.PlayerShip _ _) = return ()
