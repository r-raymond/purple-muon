module Server.MainLoop
    ( initLoop
    ) where

import           Protolude

import qualified Server.Frames as SFR
import qualified Server.Types  as STY

initLoop :: STY.Server ()
initLoop = loop

loop :: STY.Server ()
loop = do
    SFR.frameBegin

    modify update

    sendNetwork

    SFR.manageFps

update :: STY.ServerState -> STY.ServerState
update (STY.WaitingForConnections t) = STY.WaitingForConnections t
update (STY.InGame gs) = STY.InGame $
    CLE.over STY.pObjs
             (\x -> PPC.integrateTimeStep PPC.g PPC.physicsStep x DIS.empty)
             gs

sendNetwork :: STY.Server ()
sendNetwork = do
    st <- get
    case st of
      -- TODO

loop :: MonadIO m => NSO.Socket -> NSO.SockAddr -> PPT.PhysicalObjects -> m ()
loop s a o = do
    STF.frameBegin

    -- Update objs
    let newObjs = PPA.integrateTimeStep PPC.g PPC.physicsStep o DIS.empty
        toSend  = toS $ CCZ.compress $ DBI.encode $ DIS.toList newObjs
        l       = DBS.length toSend

    void $ liftIO $ NSB.sendTo s (PNT.unUUID uuid <> toSend) a

    STF.manageFps

    loop s a newObjs
