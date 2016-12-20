module Client.Init
    ( withGraphics
    ) where

import Protolude

import qualified SDL.Init                  as SIN
import qualified SDL.Video                 as SVI
import qualified Control.Exception         as CEX

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

-- | Run a computation with SDL, SDL window and SDL renderer
withGraphics :: (SVI.Window -> SVI.Renderer -> IO ()) -> IO (Either SomeException ())
withGraphics comp = try $ do
    withSDL (withSDLWindow (\x -> withSDLRenderer x comp))

