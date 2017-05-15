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

data SoundEffect = SoundShoot | SoundExplosion

sendCommand server action =
  atomically $ writeTChan (serverCommandChannel server) action

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

withSoundServer action = do
  server <- createServer
  action server `finally` destroyServer server

data SoundServer =
  SoundServer { serverCommandChannel :: TChan (UDP -> IO ())
              , serverProcessThreadId :: ThreadId
              , serverClientThreadId :: ThreadId
              , serverUdpCon :: UDP
              }

createServer = do
  serverCommandChannel <- newTChanIO
  serverProcessThreadId <-
    forkIO $ do
      callCommand "csound -odac osc_receive.csd"
  serverUdpCon <- openUDP "127.0.0.1" 7770
  serverClientThreadId <-
    forkIO $ runServer serverCommandChannel serverUdpCon
  return SoundServer {..}

destroyServer s = do
  mapM_ (killThread . (flip ($) s))
    [serverProcessThreadId, serverClientThreadId]
  close $ serverUdpCon s

runServer chan udp =
  go
  where
    go = do
      action <- atomically $ readTChan chan
      _ <- action udp
      go
