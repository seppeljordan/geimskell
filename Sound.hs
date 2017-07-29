{-# LANGUAGE RecordWildCards #-}

module Sound where

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Exception
import Control.Monad.STM
import Sound.OSC.Transport.FD
import Sound.OSC.Transport.FD.UDP
import Sound.OSC.Type
import System.Process

import Paths_geimskell

data SoundEffect = SoundShoot | SoundExplosion

sendCommand EmptySoundServer _ = return ()
sendCommand (SoundServer { serverCommandChannel = commandChannel }
            ) action =
  atomically $ writeTChan commandChannel action

playSoundEffect server eff =
  sendCommand server action
  where
    action trans = sendMessage trans $
      message
      ( case eff of
          SoundShoot -> "/shoot"
          SoundExplosion -> "/explosion"
      )
      [Float 66.6]

withEmptySoundServer action = action EmptySoundServer

withCsoundServer action = do
  server <- createServer
  action server `finally` destroyServer server

data SoundServer =
  SoundServer { serverCommandChannel :: TChan (UDP -> IO ())
              , serverProcessThreadId :: ThreadId
              , serverClientThreadId :: ThreadId
              , serverUdpCon :: UDP
              } |
  EmptySoundServer

createServer = do
  serverCommandChannel <- newTChanIO
  serverProcessThreadId <-
    forkIO $ do
      soundAssetFile <-
        getDataFileName "assets/audio/osc_receive.csd"
      callCommand $
        "csound -odac " ++ soundAssetFile
  serverUdpCon <- openUDP "127.0.0.1" 7770
  serverClientThreadId <-
    forkIO $ runServer serverCommandChannel serverUdpCon
  return SoundServer {..}

destroyServer s@(SoundServer { serverUdpCon = udpCon
                             , serverProcessThreadId = processThread
                             , serverClientThreadId = clientThread}
                ) = do
  mapM_ killThread [processThread, clientThread]
  close udpCon
desroyServer EmptySoundServer = return ()

runServer chan udp =
  go
  where
    go = do
      action <- atomically $ readTChan chan
      _ <- action udp
      go
