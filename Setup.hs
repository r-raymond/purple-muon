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
    writeFile "common/Version.hs" versionModule

template1 = "\
\module Version\n\
\    ( gitTag\n\
\    , platform\n\
\    , compiler\n\
\    ) where\n\
\ \n\
\import Protolude\n\
\ \n\
\gitTag :: Text\n\
\gitTag = \""

template2 = "\"\n\
\ \n\
\platform :: Text\n\
\platform = \""

template3 = "\"\n\
\ \n\
\compiler :: Text\n\
\compiler = \""

template4 = "\"\n"
