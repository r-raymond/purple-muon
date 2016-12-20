module Event
    (
    ) where

import Protolude

import qualified SDL.Event as SEV

handleEvent :: SEV.Event -> Game ()
handleEvent ev = case (SEV.eventPayload ev) of
    SEV.KeyboardEvent (SEV.KeyboardEventData _ SEV.Pressed _ (SIK.Keysym SIK.ScancodeEscape _ _)) -> (put (CTY.AppState False)) -- TODO: do this via lenses
    _ -> return ()
