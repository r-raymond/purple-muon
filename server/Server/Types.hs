module Server.Types
    (
    ) where

data ServerState
    = Idle
    | Connected

data GameState
    = GameState
    { pObjs     :: PPT.PhysicalObjects
    }
