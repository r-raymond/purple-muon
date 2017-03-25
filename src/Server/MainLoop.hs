module Server.MainLoop
    ( initLoop
    ) where

import           Protolude

import qualified Control.Concurrent.STM       as CCS
import qualified Control.Lens                 as CLE
import qualified Control.Lens.Zoom            as CLZ
import qualified Data.Binary                  as DBI
import qualified Data.IntMap.Strict           as DIS
import qualified Network.Socket.ByteString    as NSB
import qualified System.Log.FastLogger        as SLF

import qualified PurpleMuon.Network.Types     as PNT
import qualified PurpleMuon.Network.Util      as PNU
import qualified PurpleMuon.Physics.Algorithm as PPA
import qualified PurpleMuon.Physics.Constants as PPC
import qualified PurpleMuon.Types             as PTY

import qualified Server.CommandLine           as SCO
import qualified Server.Config                as SCO
import qualified Server.Frames                as SFR
import qualified Server.Network               as SNE
import qualified Server.Types                 as STY


initLoop :: MonadIO m => SCO.CommandLineOptions -> m ()
initLoop clo = do
    (Right ss) <- PNU.serverSocket "7123"
    tb <- liftIO $ CCS.atomically $ CCS.newTBQueue 128
    ls <- liftIO $ SLF.newStdoutLoggerSet SLF.defaultBufSize
    let uui = toS $ DBI.encode $ SCO.uuid clo
        res = STY.Resources tb ss ls uui
    liftIO $ evalStateT (runReaderT waitingLoop res) (STY.WaitingState [])

waitingLoop :: STY.WaitingServer ()
waitingLoop = do
    res <- ask
    let s = CLE.view STY.socket res
        l = CLE.view STY.logger res
    (m, a) <- liftIO $ NSB.recvFrom s 1024
    let result = DBI.decodeOrFail $ toS $ m
    case result of
        Left _ -> do
            liftIO $ SLF.pushLogStrLn l "Discarding invalid package"
            waitingLoop
        Right (_, _, mes) ->
            case mes of
                (PNT.RequestConnection (PNT.PlayerName name)) -> liftIO $ do
                    SLF.pushLogStrLn l (SLF.toLogStr $ "Player " <> name <> " connected")
                    evalStateT (runReaderT loop res)
                               (STY.GameState SCO.initialObjs SCO.initialPhyObjs (toEnum 0) [STY.ClientConnection a name (PTY.Key 0)] 0)
                _ -> do
                    liftIO $ SLF.pushLogStrLn l "Discard valid but inappropriate package"
                    waitingLoop


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
