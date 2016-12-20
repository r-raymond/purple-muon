{-# LANGUAGE TemplateHaskell #-}

module Client.Types
    ( AppState(..), running, game
    , Game
    , GameState(..), physicalObjects
    ) where

import Protolude

import qualified Control.Lens as CLE

import qualified PurpleMuon.Physics.Types as PPT

data AppState
    = AppState
    { _running :: Bool
    , _game    :: GameState
    } deriving (Show)

type Game a = StateT AppState IO a

data GameState
    = GameState
    { _physicalObjects :: [PPT.PhysicalObject]
    } deriving (Show)

CLE.makeLenses ''AppState
CLE.makeLenses ''GameState
