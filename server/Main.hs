module Main where

import Protolude

import Network.Socket.ByteString

import PurpleMuon.Network.Util

main :: IO ()
main = do
    (Right ss) <- serverSocket "7123"
    (m, a) <- recvFrom ss 1024
    putStrLn ("Received: " ++ (toS m) ++ " from " ++ (show a))
