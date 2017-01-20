module Version
    ( gitTag
    , platform
    , compiler
    ) where
 
import Protolude
 
gitTag :: Text
gitTag = "v0.0-101-gb4632c5"
 
platform :: Text
platform = "x86_64-linux"
 
compiler :: Text
compiler = "ghc-8.0.1"
