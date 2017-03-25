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

module Server.Main
    ( main
    , initLoop
    ) where

import           Protolude

import qualified Control.Concurrent.STM  as CCS
import qualified Data.Binary             as DBI
import qualified Options.Applicative     as OAP
import qualified System.Log.FastLogger   as SLF

import qualified PurpleMuon.Network.Util as PNU

import qualified Server.CommandLine      as SCO
import qualified Server.Types            as STY
import qualified Server.WaitingLoop      as SWA

-- | Initialize the loop state
initLoop :: MonadIO m => SCO.CommandLineOptions -> m ()
initLoop clo = do
    (Right ss) <- PNU.serverSocket "7123"
    tb <- liftIO $ CCS.atomically $ CCS.newTBQueue 128
    ls <- liftIO $ SLF.newStdoutLoggerSet SLF.defaultBufSize
    let uui = toS $ DBI.encode $ SCO.uuid clo
        res = STY.Resources tb ss ls uui
    liftIO $ evalStateT (runReaderT SWA.waitingLoop res) (STY.WaitingState [])

main :: IO ()
main = OAP.execParser opts >>= initLoop
  where
    opts = OAP.info (OAP.helper OAP.<*> SCO.parser)
        ( OAP.fullDesc
       <> OAP.progDesc "Run a purple muon server"
       <> OAP.header "pm-server (C) 2017 Robin Raymond GPL-3")
