module Main where

import           Protolude

import qualified Control.Concurrent        as CCO
import qualified Control.Exception         as CEX
import qualified Data.AdditiveGroup        as DAD
import qualified Data.AffineSpace          as DAF
import qualified Data.Thyme.Clock          as DTC
import qualified Foreign.C.Types           as FCT
import           Network.Socket.ByteString
import qualified SDL                       as SDL
import qualified SDL.Event                 as SEV
import qualified SDL.Init                  as SIN
import qualified SDL.Input.Keyboard        as SIK
import qualified SDL.Vect                  as SVE
import qualified SDL.Video                 as SVI

import           PurpleMuon.Network.Util

main :: IO ()
main = do
    (Right cs) <- clientSocket "127.0.0.1" "7123"
    _ <- send cs "Hello World"
    r <- withGraphics (\x y -> (evalStateT (loop x y) (AppState True)))
    case r of
        Left ex  -> putStrLn ("Error: " <> (show ex) :: Text)
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

data AppState
    = AppState
    { running :: Bool
    } deriving (Show)


type Game a = StateT AppState IO a

loop :: SVI.Window -> SVI.Renderer -> Game ()
loop window renderer = do
    start <- liftIO $ DTC.getCurrentTime

    SEV.mapEvents handleEvent
    SVI.rendererDrawColor renderer SDL.$= SVE.V4 0 0 0 0
    SVI.clear renderer

    SVI.rendererDrawColor renderer SDL.$= SVE.V4 255 0 0 0
    let help = fmap FCT.CInt (SVE.V2 10 10)
    SVI.fillRect renderer (Just (SVI.Rectangle (SVE.P help) help))

    SVI.present renderer
    end <- liftIO $ DTC.getCurrentTime
    let elapsed = end DAF..-. start
    when (elapsed < minLoopTime) (waitFor (minLoopTime DAD.^-^ elapsed))
    whenM (fmap running get) (loop window renderer)

waitFor :: MonadIO m => DTC.NominalDiffTime -> m ()
waitFor dt = liftIO $ CCO.threadDelay dt_ms
  where
    dt_s = DTC.toSeconds dt :: Float
    dt_ms_float = dt_s * 1000000
    dt_ms = truncate dt_ms_float

handleEvent :: SEV.Event -> Game ()
handleEvent ev = case (SEV.eventPayload ev) of
    SEV.KeyboardEvent (SEV.KeyboardEventData _ SEV.Pressed _ (SIK.Keysym SIK.ScancodeEscape _ _)) -> (put (AppState False)) -- TODO: do this via lenses
    _ -> return ()
