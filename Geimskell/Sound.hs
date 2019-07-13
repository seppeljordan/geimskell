{-# LANGUAGE RecordWildCards #-}

module Geimskell.Sound
  ( SoundServer
  , SoundEffect (SoundShoot, SoundExplosion)
  , withCsoundServer
  , withEmptySoundServer
  , playSoundEffect
  )
where

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Exception
import Control.Monad.STM
import Sound.OSC.Datum
import Sound.OSC.Packet
import Sound.OSC.Transport.FD
import Sound.OSC.Transport.FD.UDP
import System.Process

import Paths_geimskell (getDataFileName)

data SoundEffect = SoundShoot | SoundExplosion

type ServerMessage = UDP -> IO ()

data SoundServer =
  SoundServer { serverCommandChannel :: TChan ServerMessage
              , serverProcessThreadId :: ThreadId
              , serverClientThreadId :: ThreadId
              , serverUdpCon :: UDP
              } |
  EmptySoundServer

sendCommand :: SoundServer -> ServerMessage -> IO ()
sendCommand EmptySoundServer _ = return ()
sendCommand (SoundServer { serverCommandChannel = commandChannel }
            ) action =
  atomically $ writeTChan commandChannel action

playSoundEffect :: SoundServer -> SoundEffect -> IO ()
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

withEmptySoundServer :: (SoundServer -> a) -> a
withEmptySoundServer action = action EmptySoundServer


withCsoundServer :: (SoundServer -> IO a) -> IO a
withCsoundServer action = do
  server <- createServer
  action server `finally` destroyServer server

createServer :: IO SoundServer
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

destroyServer :: SoundServer -> IO ()
destroyServer (SoundServer { serverUdpCon = udpCon
                           , serverProcessThreadId = processThread
                           , serverClientThreadId = clientThread}
              ) = do
  mapM_ killThread [processThread, clientThread]
  close udpCon
destroyServer EmptySoundServer = return ()

runServer :: TChan ServerMessage -> UDP -> IO ()
runServer chan udp =
  go
  where
    go = do
      action <- atomically $ readTChan chan
      _ <- action udp
      go
