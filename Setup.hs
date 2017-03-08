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

import           Control.Exception
import           Data.Char
import           Data.List
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Text
import           System.Exit
import           System.Process

main = defaultMainWithHooks $ simpleUserHooks
    { postConf = postConfHook
    }

trim = dropWhileEnd isSpace . dropWhile isSpace

postConfHook _ _ _ buildInfo = do
    r <- try $ readProcessWithExitCode "git" ["describe"] ""
                :: IO (Either SomeException (ExitCode, String, String))

    let cc = display $ compilerId $ compiler buildInfo
        hp = display $ hostPlatform buildInfo
        ve = case r of
                Right (ec, stdout, _) -> case ec of
                                            ExitSuccess -> trim stdout
                                            _           -> "UNREGISTERED"
                _                     -> "UNREGISTERED"
        versionModule = template1 ++ ve ++ template2 ++ hp ++ template3 ++ cc
                        ++ template4
    writeFile "src/Version.hs" versionModule

template1 = "\
\module Version\n\
\    ( gitTag\n\
\    , platform\n\
\    , compiler\n\
\    ) where\n\
\ \n\
\import Protolude\n\
\ \n\
\-- | The current gitTag as given by `git describe`, or `UNKNOWN` if this\n\
\-- command fails.\n\
\gitTag :: Text\n\
\gitTag = \""

template2 = "\"\n\
\ \n\
\-- | The platform this was build on.\n\
\platform :: Text\n\
\platform = \""

template3 = "\"\n\
\ \n\
\-- | The compiler that was used for this build.\n\
\compiler :: Text\n\
\compiler = \""

template4 = "\"\n"
