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

module Server.Main where

import           Protolude

import qualified Control.Concurrent           as CCO
import qualified Data.AdditiveGroup           as DAD
import qualified Data.AffineSpace             as DAF
import qualified Data.Binary                  as DBI
import qualified Data.ByteString              as DBS
import qualified Data.IntMap.Strict           as DIS
import qualified Data.Thyme.Clock             as DTC
import qualified Linear.V2                    as LV2
import qualified Network.Socket               as NSO
import qualified Network.Socket.ByteString    as NSB

import qualified PurpleMuon.Network.Types     as PNT
import qualified PurpleMuon.Network.Util      as PNU
import qualified PurpleMuon.Physics.Algorithm as PPA
import qualified PurpleMuon.Physics.Constants as PPC
import qualified PurpleMuon.Physics.Types     as PPT

uuid :: PNT.UUID
uuid = PNT.UUID "Test"

main :: IO ()
main = do
    (Right ss) <- PNU.serverSocket "7123"
    (m, a) <- NSB.recvFrom ss 1024
    putStrLn ("Received: " ++ toS m ++ " from " ++ show a)
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
    b <- liftIO DTC.getCurrentTime
    -- Update objs
    let newObjs = PPA.integrateTimeStep PPC.g PPC.physicsStep o DIS.empty
        toSend  = toS $ DBI.encode $ DIS.toList newObjs
        l       = DBS.length toSend
    liftIO $ print l
    void $ liftIO $ NSB.sendTo s (PNT.unUUID uuid <> toSend) a

    e <- liftIO DTC.getCurrentTime

    let used = e DAF..-. b
        mft  = DTC.fromSeconds $ PPT.unDeltaTime PPC.physicsStep
    when (used < mft) (waitFor (mft DAD.^-^ used))

    loop s a newObjs



waitFor :: MonadIO m => DTC.NominalDiffTime -> m ()
waitFor dt = liftIO $ CCO.threadDelay dt_ms
  where
    dt_s = DTC.toSeconds dt :: PPT.FlType
    dt_ms_float = dt_s * 1000000
    dt_ms = truncate dt_ms_float

