module Server.MainLoop
    ( initLoop
    ) where

import           Protolude

import qualified Server.Types as STY

initLoop :: STY.Server ()
initLoop = loop

loop :: STY.Server ()
loop = loop

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
