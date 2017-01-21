module Version
    ( gitTag
    , platform
    , compiler
    ) where
 
import Protolude
 
gitTag :: Text
gitTag = "v0.0-102-gb2f77b6"
 
platform :: Text
platform = "x86_64-linux"
 
compiler :: Text
compiler = "ghc-8.0.1"
