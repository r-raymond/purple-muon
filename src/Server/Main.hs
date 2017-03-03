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
    ) where

import           Protolude

import qualified Options.Applicative as OAP

import qualified Server.CommandLine  as SCO
import qualified Server.MainLoop     as SMA


main :: IO ()
main = OAP.execParser opts >>= SMA.initLoop
  where
    opts = OAP.info (OAP.helper OAP.<*> SCO.parser)
        ( OAP.fullDesc
       <> OAP.progDesc "Run a purple muon server"
       <> OAP.header "pm-server (C) 2017 Robin Raymond GPL-3")
