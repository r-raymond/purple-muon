module Main where

import Protolude

import qualified Control.Exception as CEX
import Network.Socket.ByteString
import qualified SDL.Init as SIN
import qualified SDL.Video as SVI
import qualified Control.Concurrent as CCO
import qualified Data.Thyme.Clock as DTC
import qualified Data.AffineSpace as DAF
import qualified Data.AdditiveGroup as DAD

import PurpleMuon.Network.Util

main :: IO ()
main = do
    (Right cs) <- clientSocket "127.0.0.1" "7123"
    _ <- send cs "Hello World"
    r <- withGraphics loop
    case r of
        Left ex -> putStrLn ("Error: " <> (show ex) :: Text)
        Right () -> return ()
    return ()

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


minLoopTime :: DTC.NominalDiffTime
minLoopTime = DTC.fromSeconds (1 / 60 :: Float)

loop :: SVI.Window -> SVI.Renderer -> IO ()
loop window renderer = do
    start <- DTC.getCurrentTime
    SVI.clear renderer



    SVI.present renderer
    end <- DTC.getCurrentTime
    let elapsed = end DAF..-. start
    when (elapsed < minLoopTime) (waitFor (minLoopTime DAD.^-^ elapsed))
    loop window renderer

waitFor :: DTC.NominalDiffTime -> IO ()
waitFor dt = CCO.threadDelay dt_ms
  where
    dt_s = DTC.toSeconds dt :: Float
    dt_ms_float = dt_s * 1000000
    dt_ms = truncate dt_ms_float
