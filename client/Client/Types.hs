{-# LANGUAGE TemplateHaskell #-}

module Client.Types
    ( AppState(..), running, game, fps, frameBegin
    , Game
    , GameState(..), physicalObjects, dt
    , Resources(..), window, renderer
    , FpsCounter(..), 
    ) where

import Protolude

import qualified Control.Lens as CLE
import qualified SDL.Video          as SVI
import qualified Data.Thyme.Clock             as DTC

import qualified PurpleMuon.Physics.Types as PPT

data AppState
    = AppState
    { _running :: Bool
    , _game    :: GameState
    , _fps     :: FpsCounter
    , _frameBegin :: DTC.UTCTime -- TODO: figure out how to get show back on Appstate
    }

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

-- TODO: Make this more efficient. Maybe a mutable array?
data FpsCounter
    = FpsCounter
    { maxFrames :: Int
    , fpsL      :: [PPT.FlType]
    } deriving (Show)

CLE.makeLenses ''AppState
CLE.makeLenses ''GameState
CLE.makeLenses ''Resources

