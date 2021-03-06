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

module Server.CommandLine
    ( CommandLineOptions(..)
    , parser
    ) where

import           Protolude

import qualified Options.Applicative as OAP

data CommandLineOptions
    = CommandLineOptions
    { uuid :: Word32
    }

parser :: OAP.Parser CommandLineOptions
parser = CommandLineOptions
    OAP.<$> OAP.option OAP.auto
        (OAP.long "uuid"
       <> OAP.short 'u'
       <> OAP.metavar "WORD32"
       <> OAP.help "magic number for network communication")

