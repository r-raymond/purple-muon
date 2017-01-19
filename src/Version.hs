module Version
    ( gitTag
    , platform
    , compiler
    ) where
 
import Protolude
 
gitTag :: Text
gitTag = "UNREGISTERED"
 
platform :: Text
platform = "x86_64-linux"
 
compiler :: Text
compiler = "ghc-8.0.1"
