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

module Client.Init
    ( withGraphics
    ) where

import           Protolude

import qualified Control.Exception as CEX
import qualified SDL.Font          as SFO
import qualified SDL.Init          as SIN
import qualified SDL.Mixer         as SMI
import qualified SDL.Video         as SVI

-- | Run a computation with initialized SDL
-- Throws SDLExcetpion on Failure
withSDL :: IO () -> IO ()
withSDL comp = CEX.bracket_ SIN.initializeAll SIN.quit comp

-- | Run a computation with an SDL window
-- Throws SDLExcetpion on Failure
withSDLWindow :: (SVI.Window -> IO ()) -> IO ()
withSDLWindow comp = CEX.bracket (SVI.createWindow "Hello" SVI.defaultWindow)
                                 SVI.destroyWindow
                                 comp

-- | Run a computation with an SDL renderer
withSDLRenderer :: SVI.Window -> (SVI.Window -> SVI.Renderer -> IO ()) -> IO ()
withSDLRenderer w comp = CEX.bracket (SVI.createRenderer w (-1) SVI.defaultRenderer)
                                     SVI.destroyRenderer
                                     (comp w)

-- | Run a computation with SDL-ttf
withSDLTtf :: IO () -> IO ()
withSDLTtf comp = CEX.bracket_ SFO.initialize SFO.quit comp

-- | Run a computation with SDL-mixer
withSDLMixer :: IO () -> IO ()
withSDLMixer = SMI.withAudio SMI.defaultAudio 1024

-- | Run a computation with SDL, SDL window and SDL renderer, SDL-ttf and
-- SDL-mixer.
withGraphics :: (SVI.Window -> SVI.Renderer -> IO ()) -> IO (Either SomeException ())
withGraphics comp = try $ do
    withSDL
        (withSDLWindow
            (\x -> withSDLRenderer x
                        (\y z -> withSDLTtf $ withSDLMixer $ comp y z)))

