module Client.Event
    ( handleEvent
    ) where

import Protolude

import qualified Control.Lens as CLE
import qualified SDL.Input.Keyboard        as SIK
import qualified SDL.Event as SEV

import qualified Client.Types as CTY

handleEvent :: SEV.Event -> CTY.Game ()
handleEvent ev = case (SEV.eventPayload ev) of
    SEV.KeyboardEvent e -> handleKeyboardEvent e
    _ -> return ()


handleKeyboardEvent :: SEV.KeyboardEventData -> CTY.Game ()
handleKeyboardEvent (SEV.KeyboardEventData _ SEV.Pressed _ (SIK.Keysym SIK.ScancodeEscape _ _)) =
    modify (CLE.set CTY.running False)
handleKeyboardEvent _ = return ()
