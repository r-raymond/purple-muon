{-# LANGUAGE TemplateHaskell #-}

module Client.Types
    ( AppState(..), running, game, fps, frameBegin, textures
    , Game
    , GameState(..), physicalObjects, dt, accumTime
    , Resources(..), window, renderer, tbqueue
    , FpsCounter(..)
    , TextureUUIDs(..)
    ) where

import           Protolude

import qualified Control.Concurrent.STM   as CCS
import qualified Control.Lens             as CLE
import qualified Data.Thyme.Clock         as DTC
import qualified SDL.Video                as SVI

import qualified Client.Video.Types       as CVT
import qualified PurpleMuon.Network.Types as PNT
import qualified PurpleMuon.Physics.Types as PPT

data AppState
    = AppState
    { _running    :: Bool
    , _game       :: GameState
    , _fps        :: FpsCounter
    , _frameBegin :: DTC.UTCTime -- TODO: figure out how to get show back on Appstate
    , _textures   :: CVT.TextureLoader
    }

type Game a = ReaderT Resources (StateT AppState IO) a

data GameState
    = GameState
    { _physicalObjects :: PPT.PhysicalObjects
    , _dt              :: PPT.DeltaTime
    , _accumTime       :: PPT.DeltaTime         -- ^ Accumulated time for fixed physics step
    }

data Resources
    = Resources
    { _window   :: SVI.Window
    , _renderer :: SVI.Renderer
    , _tbqueue  :: CCS.TBQueue PNT.NakedMessage
    }

-- TODO: Make this more efficient. Maybe a mutable array?
data FpsCounter
    = FpsCounter
    { maxFrames :: Int
    , fpsL      :: [PPT.FlType]
    } deriving (Show)

data TextureUUIDs
    = TextureUUIDs
    { background :: CVT.TexUUID
    , stones     :: CVT.TexUUID
    }

CLE.makeLenses ''AppState
CLE.makeLenses ''GameState
CLE.makeLenses ''Resources
