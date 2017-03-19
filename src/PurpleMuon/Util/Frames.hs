
--  Copyright 2016, 2017 Robin Raymond
--
--  This file is part of Purple Muon
--
--  Purple Muon is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  Purple Muon is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Purple Muon.  If not, see <http://www.gnu.org/licenses/>.

{-|
Module      : PurpleMuon.Util.Frames
Description : Module to handle frame mangement.
Copyright   : (c) Robin Raymond, 2016-2017
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX

This module defines helper function for frame management. It allows to define a
wanted frame length and wait if the frame was shorter. It also abstracts over
storing frame lengths and calculating FPS.
-}

module PurpleMuon.Util.Frames
    ( frameBegin
    , manageFps
    ) where

import           Protolude

import qualified Control.Concurrent       as CCO
import qualified Data.AdditiveGroup       as DAD
import qualified Data.AffineSpace         as DAF
import qualified Data.Thyme.Clock         as DTC

import qualified PurpleMuon.Types         as PTY

-- | Call at the beginning of a frame.
-- Updates the frameBegin timer.
frameBegin :: MonadIO m
           => (DTC.UTCTime -> m ())     -- ^ function to store the time
           -> m ()
frameBegin f = do
    b <- liftIO $ DTC.getCurrentTime
    f b

-- | Sleep for a given time
waitFor :: MonadIO m => DTC.NominalDiffTime -> m ()
waitFor dt = liftIO $ CCO.threadDelay dt_ms
  where
    dt_s = DTC.toSeconds dt :: PTY.FlType
    dt_ms_float = dt_s * 1000000
    dt_ms = truncate dt_ms_float

-- | Handle frame time.
-- The function will then calculate the remaining time, sleep the thread,
-- update the frame counter and the dt variable of the game state
manageFps :: MonadIO m
          => DTC.NominalDiffTime     -- ^ the desired frame time
          -> m DTC.UTCTime           -- ^ get the frame begin
          -> (PTY.FlType -> m ())    -- ^ store `dt`
          -> m ()
manageFps minFT getFB storeDt = do
    end <- liftIO $ DTC.getCurrentTime
    start <- getFB
    let used = end DAF..-. start

    when (used < minFT) (waitFor (minFT DAD.^-^ used))
    final <- liftIO $ DTC.getCurrentTime
    let elapsed = final DAF..-. start
        frameTime = DTC.toSeconds elapsed :: PTY.FlType

    storeDt frameTime
