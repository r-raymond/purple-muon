module Main where

import Protolude

import Network.Socket.ByteString

import PurpleMuon.Network.Util

main :: IO ()
main = do
    (Right cs) <- clientSocket "127.0.0.1" "7123"
    _ <- send cs "Hello World"
    return ()
