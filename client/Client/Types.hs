{-# LANGUAGE TemplateHaskell #-}

module Client.Types
    ( AppState(..), running, game
    , Game
    , GameState(..), physicalObjects, dt
    , Resources(..), window, renderer
    ) where

import Protolude

import qualified Control.Lens as CLE
import qualified SDL.Video          as SVI

import qualified PurpleMuon.Physics.Types as PPT

data AppState
    = AppState
    { _running :: Bool
    , _game    :: GameState
    } deriving (Show)

type Game a = ReaderT Resources (StateT AppState IO) a

data GameState
    = GameState
    { _physicalObjects :: PPT.PhysicalObjects
    , _dt              :: PPT.DeltaTime
    } deriving (Show)

data Resources
    = Resources 
    { _window :: SVI.Window
    , _renderer :: SVI.Renderer
    } deriving (Show)

CLE.makeLenses ''AppState
CLE.makeLenses ''GameState
CLE.makeLenses ''Resources
