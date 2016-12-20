module Client.Event
    ( handleEvent
    ) where

import Protolude

import qualified SDL.Input.Keyboard        as SIK
import qualified SDL.Event as SEV

import qualified Client.Types as CTY

handleEvent :: SEV.Event -> CTY.Game ()
handleEvent ev = case (SEV.eventPayload ev) of
    SEV.KeyboardEvent (SEV.KeyboardEventData _ SEV.Pressed _ (SIK.Keysym SIK.ScancodeEscape _ _)) -> (put (CTY.AppState False)) -- TODO: do this via lenses
    _ -> return ()
