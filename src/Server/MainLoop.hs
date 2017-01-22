module Server.MainLoop
    ( initLoop
    ) where

import           Protolude

import qualified Codec.Compression.Zlib       as CCZ
import qualified Control.Concurrent.STM       as CCS
import qualified Control.Lens                 as CLE
import qualified Data.Binary                  as DBI
import qualified Data.ByteString              as DBY
import qualified Data.IntMap.Strict           as DIS
import qualified Network.Socket.ByteString    as NSB
import qualified System.Log.FastLogger        as SLF

import qualified PurpleMuon.Network.Types     as PNT
import qualified PurpleMuon.Network.Util      as PNU
import qualified PurpleMuon.Physics.Algorithm as PPA
import qualified PurpleMuon.Physics.Constants as PPC

import qualified Server.Config                as SCO
import qualified Server.Frames                as SFR
import qualified Server.Types                 as STY

uuid :: PNT.UUID
uuid = PNT.UUID "Test"

initLoop :: MonadIO m => m ()
initLoop = do
    (Right ss) <- PNU.serverSocket "7123"
    tb <- liftIO $ CCS.atomically $ CCS.newTBQueue 128
    ls <- liftIO $ SLF.newStdoutLoggerSet SLF.defaultBufSize
    let res = STY.Resources tb ss ls
    liftIO $ evalStateT (runReaderT waitingLoop res) STY.WaitingState

waitingLoop :: STY.WaitingServer ()
waitingLoop = do
    res <- ask
    let s = CLE.view STY.socket res
        l = CLE.view STY.logger res
    (_, a) <- liftIO $ NSB.recvFrom s 1024
    liftIO $ SLF.pushLogStrLn l "Client connected"
    liftIO $ evalStateT (runReaderT loop res)
                        (STY.GameState SCO.initialObjs (toEnum 0) [a])

loop :: STY.Server ()
loop = do
    SFR.frameBegin

    modify update

    sendNetwork

    SFR.manageFps

    loop

update :: STY.GameState -> STY.GameState
update = CLE.over STY.pObjs
             (\x -> PPA.integrateTimeStep PPC.g PPC.physicsStep x DIS.empty)

sendNetwork :: STY.Server ()
sendNetwork = do
    st <- get
    res <- ask
    let toSend = toS $ CCZ.compress $ DBI.encode $ DIS.toList (CLE.view STY.pObjs st)
        socket = CLE.view STY.socket res
        logger = CLE.view STY.logger res
        send a = liftIO $ NSB.sendTo socket (PNT.unUUID uuid <> toSend) a
        clients = CLE.view STY.clients st

    liftIO $ SLF.pushLogStrLn logger (SLF.toLogStr ("Sending update package. Size: "
                                    <> (show $ DBY.length toSend)
                                    <> "; Clients: "
                                    ++  (show $ length clients)))
    sequence_ (fmap send clients)
