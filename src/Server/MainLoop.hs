module Server.MainLoop
    ( loop
    ) where

import           Protolude

import qualified Control.Lens                 as CLE
import qualified Control.Lens.Zoom            as CLZ
import qualified Data.IntMap.Strict           as DIS

import qualified PurpleMuon.Network.Types     as PNT
import qualified PurpleMuon.Physics.Algorithm as PPA
import qualified PurpleMuon.Physics.Constants as PPC

import qualified Server.Frames                as SFR
import qualified Server.Network               as SNE
import qualified Server.Types                 as STY


loop :: STY.Server ()
loop = do
    SFR.frameBegin

    modify update

    sendUpdate

    SFR.manageFps

    loop

update :: STY.GameState -> STY.GameState
update = (CLE.over STY.pObjs
             (\x -> PPA.integrateTimeStep PPC.g PPC.physicsStep x DIS.empty)) .
             (CLE.over STY.intStep (+1))

sendUpdate :: STY.Server ()
sendUpdate = do
    st <- get
    CLZ.zoom (STY.clients) $ do
        SNE.sendPackageToAll (PNT.Update (CLE.view STY.pObjs st))

--sendNetwork :: STY.Server ()
--sendNetwork = do
--    st <- get
--    res <- ask
--    let toSend = toS $ CCZ.compress $ DBI.encode $ DIS.toList (CLE.view STY.pObjs st)
--        socket = CLE.view STY.socket res
--        logger = CLE.view STY.logger res
--        send a = liftIO $ NSB.sendTo socket (uuid <> toSend) a
--        clients = CLE.view STY.clients st
--
--    liftIO $ SLF.pushLogStrLn logger (SLF.toLogStr ("Sending update package. Size: "
--                                    <> (show $ DBY.length toSend)
--                                    <> "; Clients: "
--                                    ++  (show $ length clients)))
--    sequence_ (fmap send (fmap (CLE.view STY.addr) clients))
