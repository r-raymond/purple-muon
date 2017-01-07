module Main where

import           Protolude

import qualified Control.Concurrent           as CCO
import qualified Data.Binary                  as DBI
import qualified Data.ByteString              as DBS
import qualified Data.IntMap.Strict           as DIS
import qualified Linear.V2                    as LV2
import qualified Network.Socket               as NSO
import qualified Network.Socket.ByteString    as NSB

import qualified PurpleMuon.Network.Util      as PNU
import qualified PurpleMuon.Physics.Algorithm as PPA
import qualified PurpleMuon.Physics.Constants as PPC
import qualified PurpleMuon.Physics.Types     as PPT

clientAddr :: NSO.SockAddr
clientAddr = NSO.SockAddrInet 7124 (NSO.tupleToHostAddress (127, 0, 0, 1))

main :: IO ()
main = do
    (Right ss) <- PNU.serverSocket "7123"
    (m, a) <- NSB.recvFrom ss 1024
    putStrLn ("Received: " ++ (toS m) ++ " from " ++ (show a))
    loop ss a objs

objs :: PPT.PhysicalObjects
objs = DIS.fromList
        [ (1, PPT.PhysicalObject 1 (PPT.Mass 1) (PPT.Position (LV2.V2 0.5 0.5)) (PPT.Velocity (LV2.V2 0 (-0.1)))  PPT.NonStatic PPT.Gravitating)
        , (2, PPT.PhysicalObject 2 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.5)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (3, PPT.PhysicalObject 3 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.15)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (4, PPT.PhysicalObject 4 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.25)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (5, PPT.PhysicalObject 5 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.35)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (6, PPT.PhysicalObject 6 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.45)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)
        , (7, PPT.PhysicalObject 7 (PPT.Mass 1) (PPT.Position (LV2.V2 0.75 0.55)) (PPT.Velocity (LV2.V2 0 0.1)) PPT.NonStatic PPT.Gravitating)]

loop :: MonadIO m => NSO.Socket -> NSO.SockAddr -> PPT.PhysicalObjects -> m ()
loop s a o = do
    liftIO $ CCO.threadDelay 20000
    let newObjs = PPA.integrateTimeStep PPC.g (PPT.DeltaTime 0.02) o DIS.empty
        toSend  = toS $ DBI.encode $ DIS.toList newObjs
        l       = DBS.length toSend
    liftIO $ print l
    void $ liftIO $ NSB.sendTo s toSend a
    loop s a newObjs
