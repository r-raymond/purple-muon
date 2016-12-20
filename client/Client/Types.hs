{-# LANGUAGE TemplateHaskell #-}

module Client.Types
    ( AppState(..), running
    ) where

import Protolude

import qualified Control.Lens as CLE

data AppState
    = AppState
    { _running :: Bool
    } deriving (Show)

CLE.makeLenses ''AppState
